-- | To make report use function report. Pass fields for report with conditions
-- @
-- reportTemplate = report [
--   ("xrow", equal "123"),   -- ^ Condition on column
--   ("namerow", all),        -- ^ No condition
--   ("some", equal "bar")]   -- ^ Condition on hstore field
-- rs <- transaction con $ generate test reportTemplate
-- @
module Database.PostgreSQL.Report (
    Report(..),
    report,
    generate
    ) where

import Control.Arrow
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Monoid
import Database.PostgreSQL.Sync
import Database.PostgreSQL.Simple
import Data.List
import Data.Maybe
import Data.String

-- | Report template
data Report = Report {
    reportTables :: [String],
    reportFields :: [String],
    reportConditionts :: [Condition] }
        deriving (Show)

-- | Create report template
report :: [String] -> [String] -> [Condition] -> Report
report = Report

generate :: Report -> TIO [[FieldValue]]
generate (Report ts fs cs) = connection >>= generate' where
    generate' con = liftIO $ query con q vs' where
        (Condition cs' vs') = mconcat cs
        q = fromString $ "select " ++ intercalate ", " fs ++ " from " ++ intercalate ", " ts ++ (if null cs then "" else " where " ++ cs')

-- | Generate report (with joins if neccessary)
{-
generate :: [Sync] -> Report -> TIO [[FieldValue]]
generate ss r = connection >>= generate' where    
    generate' con = liftIO $ query_ con q where
        (tbls, names) = first nub $ unzip $ map (toName . fst) (reportFields r)
        toName c = maybe (error $ "No such column in model: " ++ c) id $ lookup c columnMap
        columnMap = concatMap toColumns ss
        toColumns stbl@(Sync _ _ g gs cs) = map (second $ addRelation (relation stbl)) $ map (id &&& garbageColumn g) gs ++ map (syncKey &&& syncColumn) cs where
            addRelation = (,)
        -- toColumns stbl@(Sync _ _ g gs cs) = zip (repeat (relation stbl)) (map (garbageColumn g) gs ++ map syncColumn cs)
        garbageColumn g gc = g ++ " -> '" ++ C8.unpack (escapeHStore (C8.pack gc)) ++ "'"
        conds = relationConds ++ (catMaybes $ zipWith (\ n r' -> snd r' n) names (reportFields r))
        relationConds = concatMap toRelationCondition tbls where
            toRelationCondition (TableRelation tblName tblIdxs) = mapMaybe toRelation tblIdxs where
                toRelation (idxName, idxTable) = fmap stringizeRelation $ find ((== idxTable) . syncTable) ss where
                    stringizeRelation snc = tblName ++ "." ++ idxName ++ " = " ++ idxTable ++ "." ++ syncId snc
        q = fromString $ "select " ++ intercalate ", " names ++ " from " ++ intercalate ", " (map tableName tbls) ++ conds' where
            conds' = if null conds then "" else " where " ++ intercalate " and " conds
-}

{-
-- | Generate report
generate :: Sync -> Report -> TIO [[FieldValue]]
generate s@(Sync tbl icol g _ rs) r = connection >>= generate' where
    generate' con = liftIO $ query_ con q where
        names = map (toName . fst) (reportFields r)
        toName c = case find ((== c) . syncKey) rs of
            Just (SyncField k c t) -> c
            Nothing -> g ++ " -> '" ++ C8.unpack (escapeHStore (C8.pack c)) ++ "'"
        conds = catMaybes $ zipWith (\ n r' -> snd r' n) names (reportFields r)
        q = fromString $ "select " ++ intercalate ", " names ++ " from " ++ tbl ++ conds' where
            conds' = if null conds then "" else " where " ++ intercalate " and " conds
-}
