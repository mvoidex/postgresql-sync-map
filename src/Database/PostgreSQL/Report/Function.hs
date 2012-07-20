module Database.PostgreSQL.Report.Function (
    ReportFunction(..),
    uses,
    constFunction, onString, onField,
    function, functionMaybe, functionFields
    ) where

import Data.List
import qualified Data.Map as M
import Database.PostgreSQL.Sync.Types

-- | Report function
data ReportFunction = ReportFunction {
    reportFunctionName :: String,
    reportFunctionImplicits :: [String],
    reportFunction :: M.Map String FieldValue -> [FieldValue] -> Maybe FieldValue }
    -- ^ Accepts all fields by name (for manual access), primary field and aruments (field values or constants)

-- | Declare that function implicitly uses some field
uses :: [String] -> ReportFunction -> ReportFunction
uses fns r = r { reportFunctionImplicits = nub (reportFunctionImplicits r ++ fns) }

-- | Constant function with no arguments
constFunction :: String -> (M.Map String FieldValue -> Maybe FieldValue) -> ReportFunction
constFunction n f = ReportFunction n [] f' where
    f' as _ = f as

-- | Pure function on string values
onString :: String -> (String -> String) -> ReportFunction
onString n f = ReportFunction n [] f' where
    f' _ [StringValue s] = Just $ StringValue (f s)
    f' _ _ = Nothing

-- | Pure function on FieldValue
onField :: String -> (FieldValue -> FieldValue) -> ReportFunction
onField n f = ReportFunction n [] f' where
    f' _ [v] = Just $ f v
    f' _ _ = Nothing

-- | Function on several arguments
function :: String -> ([FieldValue] -> FieldValue) -> ReportFunction
function n f = ReportFunction n [] f' where
    f' _ vs = Just $ f vs

-- | Function on several arguments that may not return result
functionMaybe :: String -> ([FieldValue] -> Maybe FieldValue) -> ReportFunction
functionMaybe n f = ReportFunction n [] f' where
    f' _ vs = f vs

-- | Function on several arguments with additional argument - all fields in row
functionFields :: String -> (M.Map String FieldValue -> [FieldValue] -> Maybe FieldValue) -> ReportFunction
functionFields s = ReportFunction s []
