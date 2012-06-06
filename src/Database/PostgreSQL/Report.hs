-- | To make report use function report. Pass fields for report with conditions
-- @
-- reportTemplate = report [
--   ("xrow", equal "123"),   -- ^ Condition on column
--   ("namerow", all),        -- ^ No condition
--   ("some", equal "bar")]   -- ^ Condition on hstore field
-- rs <- transaction con $ generate test reportTemplate
-- @
module Database.PostgreSQL.Report (
    Report(..), Condition,
    equal, anyValue,
    report,
    generate
    ) where

import Control.Arrow
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Sync
import Database.PostgreSQL.Simple
import Data.List
import Data.Maybe
import Data.String

-- | Report template
data Report = Report {
    reportFields :: [(String, Condition)] }

-- | Condition on field
type Condition = String -> Maybe String

-- | Field equals to specified value
equal :: String -> Condition
equal s = \ n -> Just (n ++ " = " ++ s)

-- | Any valud
anyValue :: Condition
anyValue = \ n -> Nothing

-- | Create report template with list of pairs (field, condition)
report :: [(String, Condition)] -> Report
report = Report

data TableRelation = TableRelation {
    tableName :: String,
    tableReferences :: [(String, String)] }
        deriving (Eq, Ord, Read, Show)

-- | Create table relation from Sync
relation :: Sync -> TableRelation
relation (Sync tbl _ _ _ cons) = TableRelation tbl refs where
    refs = mapMaybe toReference cons
    toReference (SyncField k c (Right s)) = Just (c, s)
    toReference _ = Nothing

-- | Generate report (with joins if neccessary)
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
