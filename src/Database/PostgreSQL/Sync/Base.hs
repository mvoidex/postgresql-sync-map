module Database.PostgreSQL.Sync.Base (
	Sync(..), SyncField(..),
	Syncs(..),
	ModelConstraint(..), Model(..), Models(..),
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
    syncIndexed :: Bool,
    syncType :: Type }                 -- ^ Type of value or reference to table

-- | Syncs table
data Syncs = Syncs {
	syncsSyncs :: M.Map String Sync,
	syncsRelations :: [Condition] }

-- | Constraint on model
data ModelConstraint = ModelConstraint {
	constraintFunction :: (SyncMap -> SyncMap,  SyncMap -> SyncMap),
	constraintCondition :: (String -> Condition) }

-- | Model based on sync
data Model = Model {
	modelName :: String,                      -- ^ Model name
	modelSync :: Sync,                        -- ^ Base sync
	modelConstraints :: [ModelConstraint] }   -- ^ Constraints on model

data Models = Models {
	-- Not good, need refactor, because all info already exists in modelsModels
	modelsSyncs :: Syncs,
	modelsModels :: M.Map String Model }

instance Show SyncField where
    show (SyncField k c i (Type st _ _)) = unwords [k, "<->", c, "::", show st, if i then "*" else ""]

-- | Condition on query, containts tables affected, condition string with placeholders ('?') and arguments
data Condition = Condition {
	conditionTablesAffected :: [String],
	conditionFieldsAffected :: [String],
	conditionString :: String,
	conditionArguments :: [Action] }
		deriving (Show)
