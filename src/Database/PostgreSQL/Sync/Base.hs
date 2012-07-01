module Database.PostgreSQL.Sync.Base (
	Sync(..), SyncField(..),
	Syncs(..),
	Condition(..)
	) where

import qualified Data.Map as M
import Database.PostgreSQL.Sync.Types

-- | Sync connectors
data Sync = Sync {
    syncTable :: String,
    syncHStore :: String,
    syncConnectors :: [SyncField] }
        deriving (Show)

-- | Field sync connector
data SyncField = SyncField {
    syncKey :: String,                 -- ^ Key in Map
    syncColumn :: String,              -- ^ Column name in database
    syncType :: Type }                 -- ^ Type of value or reference to table

-- | Syncs table
data Syncs = Syncs {
	syncsSyncs :: M.Map String Sync,
	syncsRelations :: [Condition] }

instance Show SyncField where
    show (SyncField k c (Type st _ _)) = unwords [k, "<->", c, "::", show st]

-- | Condition on query, containts tables affected, condition string with placeholders ('?') and arguments
data Condition = Condition {
	conditionTablesAffected :: [String],
	conditionFieldsAffected :: [String],
	conditionString :: String,
	conditionArguments :: [Action] }
		deriving (Show)
