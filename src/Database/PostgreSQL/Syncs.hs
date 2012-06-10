module Database.PostgreSQL.Syncs (
	Sync(..), Syncs, SyncField(..),
	Condition(..), syncsField, parseField,

	sync, syncs,
	field,
	store,
	load,

	TIO, connection,
	create,
	insert, select, update,
	transaction,

	module Database.PostgreSQL.Sync.Types
	) where

import Control.Arrow
import Database.PostgreSQL.Sync hiding (create, insert, select, update)
import qualified Database.PostgreSQL.Sync as S
import Database.PostgreSQL.Sync.Types
import qualified Data.Map as M

-- | Syncs table
type Syncs = M.Map String Sync

syncsField :: Syncs -> String -> String -> Maybe String
syncsField ss model name = fmap (\s -> condField s name) $ M.lookup model ss

-- | Parse field "model.name" to table-related field "table.column" or "table.garbage -> 'name'"
parseField :: Syncs -> String -> Maybe String
parseField ss mname = syncsField ss model name where
	(model, name) = second (drop 1) $ break (== '.') mname

-- | Make syncs
syncs :: [(String, Sync)] -> Syncs
syncs = M.fromList

-- | Create tables
create :: Syncs -> TIO ()
create = mapM_ S.create . M.elems

-- | Perform action on model
withModel :: Syncs -> String -> (Sync -> a) -> a
withModel ss name f = maybe (error "No model with name") f $ M.lookup name ss

-- | Insert Map into postgresql for model
insert :: Syncs -> String -> SyncMap -> TIO ()
insert ss name m = withModel ss name $ \s -> S.insert s m

-- | Select row by condition
select :: Syncs -> String -> Condition -> TIO SyncMap
select ss name c = withModel ss name $ \s -> S.select s c

-- | Update by condition with values, stored in map
update :: Syncs -> String -> Condition -> SyncMap -> TIO ()
update ss name c m = withModel ss name $ \s -> S.update s c m
