-- | To make report use function report. Pass fields for report with conditions
-- @
-- reportTemplate = report [
--   ("xrow", equal "123"),   -- ^ Condition on column
--   ("namerow", all),        -- ^ No condition
--   ("some", equal "bar")]   -- ^ Condition on hstore field
-- rs <- transaction con $ generate test reportTemplate
-- @
module Database.PostgreSQL.Report (
    ReportField(..),
    Report(..),
    report,
    reportc,
    generate,

    module Database.PostgreSQL.Report.Function
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Monoid
import Database.PostgreSQL.Sync
import Database.PostgreSQL.Simple
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.String
import Database.PostgreSQL.Sync.Condition
import Database.PostgreSQL.Report.Function
import Text.Regex.Posix

import Debug.Trace

-- | Report field with function
data ReportField = ReportField {
    reportFieldName :: String,
    reportFieldFunction :: Maybe (String, [String]) }
        deriving (Show)

-- | Report template
data Report = Report {
    reportTables :: [String],
    reportFields :: [ReportField],
    reportConditionts :: [Condition] }
        deriving (Show)

instance Monoid Report where
    mempty = Report [] [] []
    mappend (Report lt lf lc) (Report rt rf rc) = Report (nub $ lt ++ rt) (lf ++ rf) (lc ++ rc)

-- | Parse regex
parseRx :: String -> String -> Maybe [String]
parseRx x v = parse' (v =~ line x) where
    parse' :: (String, String, String, [String]) -> Maybe [String]
    parse' ("", _, "", groups) = Just groups
    parse' _ = Nothing

split = unfoldr splitComma where
    splitComma "" = Nothing
    splitComma s = Just . second (drop 1) . break (== ',') $ s

trim = p . p where
    p = reverse . dropWhile isSpace

identRx = "([a-zA-Z0-9_]+)"
argRx = "([^,]*)"
line s = "^" ++ s ++ "$"

simpleRx = identRx ++ "\\." ++ identRx
functionRx = identRx ++ "\\(" ++ simpleRx ++ "((," ++ argRx ++ ")*)" ++ "\\)"

-- | Parse report field
-- model.name - simple field
-- NAME(model.name) - field with function on it
-- model.name > 10 - field and condition
-- 'constant' - constant value
parseReportField :: Syncs -> String -> Report
parseReportField ss field = fromMaybe (error "Impossible happenned in parseReportField") $ foldr1 (<|>) [simple, function, conditioned, constanted] where
    simple = do
        [t, n] <- parseRx simpleRx field
        (t', n') <- syncsField ss t n
        return $ Report [t'] [ReportField (t' ++ "." ++ n') Nothing] []
    function = do
        [f, t, n, as, _, _] <- parseRx functionRx field
        (t', n') <- syncsField ss t n
        return $ Report [t'] [ReportField (t' ++ "." ++ n') (Just (f, drop 1 . map trim . split $ as))] []
    conditioned = case parseField ss field of
        Just _ -> Nothing
        Nothing -> if noTables then Nothing else Just $ Report [tryHead $ conditionTablesAffected cond] [ReportField (tryHead $ conditionFieldsAffected cond) Nothing] [cond]
        where
            cond = condition ss field []
            noTables = null $ conditionTablesAffected cond
    constanted = Just $ Report [] [ReportField ("'" ++ field ++ "'") Nothing] []

    tryHead [x] = x
    tryHead _ = error "Condition must use exactly one field"

-- | Create report template
report :: Syncs -> [String] -> [Condition] -> Report
report ss fs cs = Report ts pfs (cs ++ syncsRelations ss) where
    fs' = mapMaybe (parseField ss) fs
    ts = nub $ map fst fs'
    pfs = map (\(t, n) -> ReportField (t ++ "." ++ n) Nothing) fs'

-- | Create report by field-with-condition strings
reportc :: Syncs -> [String] -> Report
reportc ss fs = rs { reportConditionts = reportConditionts rs ++ syncsRelations ss } where
    rs = mconcat (map (parseReportField ss) fs)

generate :: Report -> Syncs -> [ReportFunction] -> M.Map String (M.Map String String) -> TIO [[FieldValue]]
generate (Report ts fs cs) ss funs dicts = connection >>= generate' where
    generate' con = liftIO $ liftM (map applyFunctions) $ traceShow q $ query con q (conditionArguments cond) where
        cond = mconcat $ filter (affects ts) cs
        q = fromString $ "select " ++ intercalate ", " (map reportFieldName fs) ++ " from " ++ intercalate ", " ts ++ toWhere cond
    applyFunctions :: [FieldValue] -> [FieldValue]
    applyFunctions fv = zipWith (apply fvs) (map reportFieldFunction fs) fv where
        fvs = M.fromList $ zip (map reportFieldName fs) fv
        apply :: M.Map String FieldValue -> Maybe (String, [String]) -> FieldValue -> FieldValue
        apply fieldValues nameArgs mainValue = fromMaybe mainValue $ do
            (name, args) <- nameArgs
            function <- find ((name ==) . reportFunctionName) funs
            let
                argValues = map toArgValue args
                toArgValue arg = fromMaybe (StringValue arg) $ do
                    (t, n) <- parseField ss arg
                    M.lookup (t ++ "." ++ n) fieldValues
            reportFunction function fieldValues mainValue argValues

{-
        apply _ Nothing v = v
        apply fieldValues (Just (name, args)) mainValue = where

        apply _ Nothing v = v
        apply _ (Just ("NAME", [])) (StringValue v) = maybe (StringValue "") StringValue $ listToMaybe $ drop 1 $ words v
        apply _ (Just ("SURNAME", [])) (StringValue v) = maybe (StringValue "") StringValue $ listToMaybe $ words v
        apply _ (Just ("LOOKUP", [d])) (StringValue v) = maybe (StringValue v) StringValue $ do
            dict <- M.lookup d dicts
            M.lookup v dict
        apply _ _ v = v
-}
