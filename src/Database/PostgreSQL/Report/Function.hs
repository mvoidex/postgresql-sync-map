module Database.PostgreSQL.Report.Function (
    ReportField(..),
    ReportCondition(..),
    Report(..),
    ReportValue(..),
    ReportFunction(..),
    uses, macro,
    constFunction, onString, onField,
    function, functionMaybe, functionFields
    ) where

import Data.List
import Data.Monoid
import qualified Data.Map as M
import Database.PostgreSQL.Sync.Types

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

-- | Report function
data ReportFunction = ReportFunction {
    reportFunctionName :: String,
    reportFunctionImplicits :: [String],
    reportFunctionMacro :: [Either String ReportField] -> Report,
    reportFunction :: M.Map String FieldValue -> [FieldValue] -> Maybe FieldValue }
    -- ^ Accepts all fields by name (for manual access), primary field and aruments (field values or constants)

instance Functor ReportValue where
    fmap f (ReportValue s as) = ReportValue s (fmap (fmap f) as)

instance Monoid Report where
    mempty = Report [] [] [] [] []
    mappend (Report lt lcs lv lc lo) (Report rt rcs rv rc ro) = Report (nub $ lt ++ rt) (nub $ lcs ++ rcs) (lv ++ rv) (lc ++ rc) (lo ++ ro)

-- | Declare that function implicitly uses some field
uses :: [String] -> ReportFunction -> ReportFunction
uses fns r = r { reportFunctionImplicits = nub (reportFunctionImplicits r ++ fns) }

nullMacro :: [Either String ReportField] -> Report
nullMacro _ = mempty

-- | Macro
macro :: ([Either String ReportField] -> Report) -> ReportFunction -> ReportFunction
macro f r = r { reportFunctionMacro = macro' } where
    macro' args = reportFunctionMacro r args `mappend` f args

-- | Constant function with no arguments
constFunction :: String -> (M.Map String FieldValue -> Maybe FieldValue) -> ReportFunction
constFunction n f = ReportFunction n [] nullMacro f' where
    f' as _ = f as

-- | Pure function on string values
onString :: String -> (String -> String) -> ReportFunction
onString n f = ReportFunction n [] nullMacro f' where
    f' _ [StringValue s] = Just $ StringValue (f s)
    f' _ _ = Nothing

-- | Pure function on FieldValue
onField :: String -> (FieldValue -> FieldValue) -> ReportFunction
onField n f = ReportFunction n [] nullMacro f' where
    f' _ [v] = Just $ f v
    f' _ _ = Nothing

-- | Function on several arguments
function :: String -> ([FieldValue] -> FieldValue) -> ReportFunction
function n f = ReportFunction n [] nullMacro f' where
    f' _ vs = Just $ f vs

-- | Function on several arguments that may not return result
functionMaybe :: String -> ([FieldValue] -> Maybe FieldValue) -> ReportFunction
functionMaybe n f = ReportFunction n [] nullMacro f' where
    f' _ vs = f vs

-- | Function on several arguments with additional argument - all fields in row
functionFields :: String -> (M.Map String FieldValue -> [FieldValue] -> Maybe FieldValue) -> ReportFunction
functionFields s = ReportFunction s [] nullMacro
