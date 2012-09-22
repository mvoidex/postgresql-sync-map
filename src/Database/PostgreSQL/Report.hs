{-# LANGUAGE OverloadedStrings #-}

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
    condition, orderBy, report,
    generate,

    module Database.PostgreSQL.Report.Function
    ) where

import Prelude hiding (log)

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
import System.Log

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
    reportConditions :: [ReportCondition],
    reportOrderBy :: [ReportField] }
        deriving (Eq, Read, Show)

data ReportValue a = ReportValue {
    reportValueFunction :: String,
    reportValueArguments :: [Either String a] }
        deriving (Eq, Read, Show)

instance Functor ReportValue where
    fmap f (ReportValue s as) = ReportValue s (fmap (fmap f) as)

instance Monoid Report where
    mempty = Report [] [] [] [] []
    mappend (Report lt lcs lv lc lo) (Report rt rcs rv rc ro) = Report (nub $ lt ++ rt) (nub $ lcs ++ rcs) (lv ++ rv) (lc ++ rc) (lo ++ ro)

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
splitByStr str inStr = go (inStr =~ str) where
    go :: (String, String, String) -> [String]
    go (_, "", "") = [inStr]
    go (b, _, a) = b : splitByStr str a

parseCondition :: String -> Maybe ReportCondition
parseCondition s = go (s =~ fieldRx) where
    go :: (String, String, String, [String]) -> Maybe ReportCondition
    go (_, "", "", []) = Nothing
    go (b, _, a, [m, n]) = Just $ ReportCondition (ReportField m n) (b : splitByStr (m ++ "." ++ n) a)
    go _ = error $ "Unable to parse condition: " ++ s

-- | Report without output, only with condition on some field
condition :: String -> Maybe Report
condition = fmap toReport . parseCondition where
    toReport rc@(ReportCondition rf@(ReportField m n) s) = Report [m] [rf] [] [rc] []

orderBy :: String -> Maybe Report
orderBy s = select $ s =~ fieldRx where
    select :: (String, String, String, [String]) -> Maybe Report
    select ("", _, "", [m, n]) = Just $ Report [m] [] [] [] [ReportField m n]
    select _ = Nothing

report :: String -> Maybe Report
report = fmap toReport . parseReportValueNull where
    toReport r = Report models fields [values] (filter (not . noCond) conditions) [] where
        conditions = rights . reportValueArguments $ r
        fields = nub $ map reportConditionField conditions
        models = nub $ map reportModel fields
        values = fmap reportConditionField r
        noCond cond = reportConditionString cond == ["", ""]

-- | Model field as report
parseModelField :: String -> Maybe Report
parseModelField s = select $ s =~ fieldRx where
    select :: (String, String, String, [String]) -> Maybe Report
    select ("", _, "", [m, n]) = Just $ Report [m] [ReportField m n] [] [] []
    select _ = Nothing

generate :: Report -> Syncs -> [ReportFunction] -> TIO [[FieldValue]]
generate r ss funs = scope "Report.generate" $ do
    log Debug "Generating report"
    con <- connection
    generate' con
    where
        generate' con = do
            log Trace $ fromString $ "Report query: " ++ q
            log Trace $ fromString $ "Report conditions: " ++ show (conditionArguments reportRel)
            liftIO $ liftM (map applyFunctions) $ query con (fromString q) (conditionArguments reportRel)
            where
                q = "select " ++ intercalate ", " fs' ++ " from " ++ intercalate ", " ts ++ condition' ++ orderby'
                
                usedFunNames = map reportValueFunction . reportValues $ r
                usedFuns = filter ((`elem` usedFunNames) . reportFunctionName) funs

                rfuns = mconcat $ mapMaybe parseModelField $ concatMap reportFunctionImplicits usedFuns
                (Report ms fs vs cs os) = r `mappend` rfuns

                -- table names, corresponding to models
                ts = map (\ mdl -> maybe (error $ "Unknown model name: " ++ show mdl) syncTable $ (M.lookup mdl (syncsSyncs ss))) ms
                toFieldStr f = maybe err (\(t, n) -> t ++ "." ++ n) $ parseField ss fstr where
                    fstr = showField f
                    err = error $ "Unknown field name: " ++ show fstr
                -- fields as they named in tables, not in models
                fs' = map toFieldStr fs
                -- conditions on fields
                cs' = map showCondition cs
                -- orderby fields
                os' = map toFieldStr os
                -- condition relations between tables
                csRels = filter (affects ts) (syncsRelations ss)
                reportRel = mconcat csRels
                -- all conditions
                allConds = cs' ++ map conditionString csRels
                -- full condition
                condition' = if null allConds then "" else " where " ++ intercalate " and " allConds
                -- orderby
                orderby' = if null os' then "" else " order by " ++ intercalate ", " os'
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
