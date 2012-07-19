module Database.PostgreSQL.Report.Function (
	ReportFunction(..),
	onString, onField,
	function, functionMaybe, functionFields
	) where

import qualified Data.Map as M
import Database.PostgreSQL.Sync.Types

-- | Report function
data ReportFunction = ReportFunction {
    reportFunctionName :: String,
    reportFunction :: M.Map String FieldValue -> FieldValue -> [FieldValue] -> Maybe FieldValue }
    -- ^ Accepts all fields by name (for manual access), primary field and aruments (field values or constants)

-- | Pure function on string values
onString :: String -> (String -> String) -> ReportFunction
onString n f = ReportFunction n f' where
	f' _ (StringValue s) _ = Just $ StringValue (f s)
	f' _ _ _ = Nothing

-- | Pure function on FieldValue
onField :: String -> (FieldValue -> FieldValue) -> ReportFunction
onField n f = ReportFunction n f' where
	f' _ v _ = Just $ f v

-- | Function on several arguments
function :: String -> (FieldValue -> [FieldValue] -> FieldValue) -> ReportFunction
function n f = ReportFunction n f' where
	f' _ v as = Just $ f v as

-- | Function on several arguments that may not return result
functionMaybe :: String -> (FieldValue -> [FieldValue] -> Maybe FieldValue) -> ReportFunction
functionMaybe n f = ReportFunction n f' where
	f' _ v as = f v as

-- | Function on several arguments with additional argument - all fields in row
functionFields :: String -> (M.Map String FieldValue -> FieldValue -> [FieldValue] -> Maybe FieldValue) -> ReportFunction
functionFields = ReportFunction
