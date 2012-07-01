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
    reportc,
    generate
    ) where

import Control.Arrow
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Monoid
import Database.PostgreSQL.Sync
import Database.PostgreSQL.Simple
import Data.Either
import Data.List
import Data.Maybe
import Data.String
import Database.PostgreSQL.Sync.Condition

-- | Report template
data Report = Report {
    reportTables :: [String],
    reportFields :: [String],
    reportConditionts :: [Condition] }
        deriving (Show)

-- | Create report template
report :: Syncs -> [String] -> [Condition] -> Report
report ss fs cs = Report ts pfs (cs ++ syncsRelations ss) where
    fs' = mapMaybe (parseField ss) fs
    ts = nub $ map fst fs'
    pfs = map (\(t, n) -> t ++ "." ++ n) fs'

-- | Create report by field-with-condition strings
-- Field with condition is one of:
-- 1. model.name - field without condition
-- 2. model.name > 10 - field and condition in one place
reportc :: Syncs -> [String] -> Report
reportc ss fs = Report ts pfs (cs ++ syncsRelations ss) where
    fs' = mapMaybe (parseField ss) fs
    ts = nub $ map fst fs' ++ concatMap conditionTablesAffected cs
    pfs = map (\(t, n) -> t ++ "." ++ n) fs' ++ concatMap conditionFieldsAffected cs
    cs = mapMaybe toC fs

    toC s = case parseField ss s of
        Nothing -> Just $ condition ss s []
        Just _ -> Nothing

generate :: Report -> TIO [[FieldValue]]
generate (Report ts fs cs) = connection >>= generate' where
    generate' con = liftIO $ query con q (conditionArguments cond) where
        cond = mconcat $ filter (affects ts) cs
        q = fromString $ "select " ++ intercalate ", " fs ++ " from " ++ intercalate ", " ts ++ toWhere cond
