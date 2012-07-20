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
    ReportCondition(..),
    Report(..),
    ReportValue(..),
    report,
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

-- | Report field
data ReportField = ReportField {
    reportModel :: String,
    reportField :: String }
        deriving (Eq, Ord, Read, Show)

data ReportCondition = ReportCondition {
    reportConditionField :: ReportField,
    reportConditionString :: [String] }
        deriving (Eq, Read, Show)

-- | Report template
data Report = Report {
    reportModels :: [String],
    reportFields :: [ReportField],
    reportValues :: [ReportValue ReportField],
    reportConditions :: [ReportCondition] }
        deriving (Eq, Read, Show)

data ReportValue a = ReportValue {
    reportValueFunction :: String,
    reportValueArguments :: [Either String a] }
        deriving (Eq, Read, Show)

instance Functor ReportValue where
    fmap f (ReportValue s as) = ReportValue s (fmap (fmap f) as)

instance Monoid Report where
    mempty = Report [] [] [] []
    mappend (Report lt lcs lv lc) (Report rt rcs rv rc) = Report (nub $ lt ++ rt) (nub $ lcs ++ rcs) (lv ++ rv) (lc ++ rc)

parseReportValueNull :: String -> Maybe (ReportValue ReportCondition)
parseReportValueNull s = parseReportValue s <|> fmap nameToNull (parseReportValue ("ID(" ++ s ++ ")")) where
    nameToNull rv = rv { reportValueFunction = "" }

parseReportValue :: String -> Maybe (ReportValue ReportCondition)
parseReportValue = fmap extract . parseRx functionRx where
    extract (name:(args:_)) = ReportValue name args' where
        args' = map toArg $ map trim $ split args
        toArg s = case parseCondition s of
            Just v -> Right v
            Nothing -> Left s

splitByStr :: String -> String -> [String]
splitByStr str inStr = case inStr =~ str of
    (_, "", "") -> [inStr]
    (b, _, a) -> b : splitByStr str a

parseCondition :: String -> Maybe ReportCondition
parseCondition s = case s =~ fieldRx of
    (_, "", "", []) -> Nothing
    (b, _, a, [m, n]) -> Just $ ReportCondition (ReportField m n) (b : splitByStr (m ++ "." ++ n) a)

parseReport :: String -> Maybe Report
parseReport = fmap toReport . parseReportValueNull where
    toReport r = Report models fields [values] (filter (not . noCond) conditions) where
        conditions = rights . reportValueArguments $ r
        fields = nub $ map reportConditionField conditions
        models = nub $ map reportModel fields
        values = fmap reportConditionField r
        noCond cond = reportConditionString cond == ["", ""]

-- | Model field as report
parseModelField :: String -> Maybe Report
parseModelField s = select $ s =~ fieldRx where
    select :: (String, String, String, [String]) -> Maybe Report
    select ("", _, "", [m, n]) = Just $ Report [m] [ReportField m n] [] []
    select _ = Nothing

report :: [String] -> Maybe Report
report = fmap mconcat . mapM parseReport

generate :: Report -> Syncs -> [ReportFunction] -> TIO [[FieldValue]]
generate r ss funs = connection >>= generate' where
    generate' con = liftIO $ liftM (map applyFunctions) $ query con q (conditionArguments $ mconcat csRels)
    
    rfuns = mconcat $ mapMaybe parseModelField $ concatMap reportFunctionImplicits funs
    (Report ms fs vs cs) = r `mappend` rfuns
    
    q = fromString $ "select " ++ intercalate ", " fs' ++ " from " ++ intercalate ", " ts ++ condition'
    -- table names, corresponding to models
    ts = map (maybe (error "Unknown model name") syncTable . (`M.lookup` (syncsSyncs ss))) ms
    toFieldStr = maybe (error "Unknown field name") (\(t, n) -> t ++ "." ++ n) . parseField ss . showField
    -- fields as they named in tables, not in models
    fs' = map toFieldStr fs
    -- conditions on fields
    cs' = map showCondition cs
    -- condition relations between tables
    csRels = filter (affects ts) (syncsRelations ss)
    -- all conditions
    allConds = cs' ++ map conditionString csRels
    -- full condition
    condition' = if null allConds then "" else " where " ++ intercalate " and " allConds
    -- TODO: Remove parseField and showField
    showField (ReportField m f) = m ++ "." ++ f
    showCondition (ReportCondition fld ins) = "(" ++ intercalate (toFieldStr fld) ins ++ ")"
    
    nullFun = onField "" id
    
    applyFunctions :: [FieldValue] -> [FieldValue]
    applyFunctions fv = map apply vs where
        args = M.fromList $ zip fs fv
        argss = M.mapKeys showField args
        apply :: ReportValue ReportField -> FieldValue
        apply (ReportValue fname fargs) = fromMaybe (StringValue "") $ do
            function <- find ((fname ==) . reportFunctionName) (nullFun : funs)
            let
                argValues = map toArgValue fargs
                toArgValue (Left s) = StringValue s
                toArgValue (Right v) = fromMaybe (error $ "Invalid argument: " ++ show v) $ M.lookup v args
            reportFunction function argss argValues

identRx = "([a-zA-Z0-9_]+)"
fieldRx = identRx ++ "\\." ++ identRx
argRx = "([^,]*)"
line s = "^" ++ s ++ "$"

argsRx = argRx ++ "((," ++ argRx ++ ")*)"

functionRx = identRx ++ "\\((" ++ argsRx ++ ")?\\)"

-- | Parse regex
parseRx :: String -> String -> Maybe [String]
parseRx x v = parse' (v =~ line x) where
    parse' :: (String, String, String, [String]) -> Maybe [String]
    parse' ("", "", "", []) = Nothing
    parse' ("", _, "", groups) = Just groups
    parse' _ = Nothing

split = unfoldr splitComma where
    splitComma "" = Nothing
    splitComma s = Just . second (drop 1) . break (== ',') $ s

trim = p . p where
    p = reverse . dropWhile isSpace

