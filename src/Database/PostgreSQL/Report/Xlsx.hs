{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Report.Xlsx (
    oneRow,
    reportDeclaration,
    generateReport,
    saveReport,
    createReport
    ) where

import Prelude hiding (log)

import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Ord
import Data.Monoid
import Data.String
import Data.Time
import Data.Time.LocalTime
import Data.Time.Clock.POSIX

import Database.PostgreSQL.Report
import Database.PostgreSQL.Sync

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Codec.Xlsx as Xlsx
import qualified Codec.Xlsx.Parser as Xlsx
import qualified Codec.Xlsx.Writer as Xlsx

import System.Log
import System.Locale

oneRow :: FilePath -> IO (Maybe (M.Map T.Text T.Text))
oneRow f = do
    x <- Xlsx.xlsx f
    runResourceT $
        Xlsx.sheetRowSource x 0 $$
        CL.peek

reportDeclaration :: (MonadLog m, MonadIO m) => FilePath -> m ([(T.Text, T.Text)], Maybe T.Text)
reportDeclaration f = do
    log Trace $ T.concat ["Loading report ", fromString f]
    x <- liftIO $ Xlsx.xlsx f
    [k, e] <- liftIO $ runResourceT $
              Xlsx.cellSource x 0 (map Xlsx.int2col [1..256]) $$
              CL.take 2
    let
        toTexts =
            map (toText . Xlsx.cdValue . Xlsx.cellData) .
            sortBy (comparing $ Xlsx.col2int . fst . Xlsx.cellIx)
        toText Nothing = T.empty
        toText (Just (Xlsx.CellText t)) = t
        toText (Just _) = T.empty
        kTexts = toTexts k
        eTexts = toTexts e
    log Debug $ T.concat ["Column names: ", T.intercalate ", " kTexts]
    log Debug $ T.concat ["Template expressions: ", T.intercalate ", " eTexts]
    let sortAndOrder = fmap removeStar $ find hasStar eTexts
    return (zip kTexts (map removeStar eTexts), sortAndOrder)
    where
        hasStar :: T.Text -> Bool
        hasStar t = T.head t == '*'

        removeStar :: T.Text -> T.Text
        removeStar t
            | hasStar t = T.tail t
            | otherwise = t

generateReport :: Syncs -> [ReportFunction] -> [(T.Text, T.Text)] -> [T.Text] -> [T.Text] -> TIO [[FieldValue]]
generateReport ss funs m conds orders = generate rpt' ss funs where
    m' = map T.unpack $ map snd m
    flds = map report m'
    rpt = fromMaybe (error $ "Unable to create report: " ++ show m) $ mconcat $ flds
    conds' = mapMaybe (condition . T.unpack) $ conds
    orders' = mapMaybe (orderBy . T.unpack) $ orders

    rpt' = rpt `mappend` (mconcat . filter inTables $ conds' ++ orders')
    inTables r = not $ null $ intersect (reportModels rpt) (reportModels r)

saveReport :: (MonadLog m, MonadIO m) => FilePath -> [T.Text] -> [[FieldValue]] -> m ()
saveReport f ts fs = liftIO getCurrentTimeZone >>= saveReport' where
    saveReport' tz = do
        log Trace $ T.concat ["Saving report to ", fromString f]
        liftIO $ Xlsx.writeXlsx f [sheet]
        where
            allRows = names : fs
            names = map (StringValue . T.unpack) ts
            sheet = Xlsx.Worksheet {
                Xlsx.wsName = T.pack "report",
                Xlsx.wsMinX = 1,
                Xlsx.wsMaxX = 1 + length ts,
                Xlsx.wsMinY = 1,
                Xlsx.wsMaxY = 1 + length allRows,
                Xlsx.wsColumns = [],
                Xlsx.wsRowHeights = M.empty,
                Xlsx.wsCells = cells }
            cells = M.unions $ zipWith row [1..] allRows
            row r rowData = M.unions $ zipWith (cell r) [1..] rowData
            cell r c d = M.singleton (c, r) (fieldValueToCell tz d)

createReport :: Syncs -> [ReportFunction] -> (T.Text -> [T.Text]) -> [T.Text] -> [T.Text] -> FilePath -> FilePath -> TIO ()
createReport ss funs superCond conds orders from to = scope "createReport" $ do
    (reportDecl, sortOrder) <- reportDeclaration from
    let
        fieldName = do
            so <- sortOrder
            r <- report $ T.unpack so
            (ReportField m n) <- listToMaybe $ reportFields r
            return $ T.pack $ m ++ "." ++ n
    fs <- generateReport ss funs reportDecl (maybe [] superCond fieldName ++ conds) (maybe orders (: orders) fieldName)
    saveReport to (map fst reportDecl) fs

fieldValueToCell :: TimeZone -> FieldValue -> Xlsx.CellData
fieldValueToCell _ (IntValue i) = cell $ Xlsx.CellText $ T.pack $ show i
fieldValueToCell _ (DoubleValue i) = cell $ Xlsx.CellDouble i
fieldValueToCell _ (BoolValue i) = cell $ Xlsx.CellText $ T.pack $ show i
fieldValueToCell _ (StringValue i) = cell $ Xlsx.CellText $ T.pack i
fieldValueToCell tz (TimeValue i) = cell $ Xlsx.CellText $ T.pack $ fmt $ toLocalTime i where
    fmt = formatTime defaultTimeLocale "%d.%m.%Y %H:%M"
    toLocalTime = utcToLocalTime tz . posixSecondsToUTCTime
{-
fieldValueToCell tz (TimeValue i) = cell $ Xlsx.CellLocalTime $ toLocalTime i where
    toLocalTime = utcToLocalTime tz . posixSecondsToUTCTime
-}
fieldValueToCell _ (HStoreValue i)
    | M.null i = cell $ Xlsx.CellText $ T.empty
    | otherwise = error $ "HStore can't be in one cell: " ++ show i

cell :: Xlsx.CellValue -> Xlsx.CellData
cell = Xlsx.CellData Nothing . Just
