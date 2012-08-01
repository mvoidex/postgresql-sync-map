{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Syncs (
    syncs,

    create,
    insert, select, exists, update, insertUpdate,

    module Database.PostgreSQL.Sync.Base,
    module Database.PostgreSQL.Sync.Types,
    module Database.PostgreSQL.Sync.Condition,
    module Database.PostgreSQL.Sync
    ) where

import Prelude hiding (log, catch)

import Control.Arrow
import Database.PostgreSQL.Sync hiding (create, insert, select, exists, update, insertUpdate)
import Database.PostgreSQL.Sync.Base
import qualified Database.PostgreSQL.Sync as S
import Database.PostgreSQL.Sync.Types
import Database.PostgreSQL.Sync.Condition
import qualified Data.Map as M
import System.Log

-- | Make syncs
syncs :: [(String, Sync)] -> [String] -> Syncs
syncs ss cs = result where
    result = Syncs (M.fromList ss) (map (\c -> conditionComplex result c []) cs)

-- | Create tables
create :: Syncs -> TIO ()
create = scope "Syncs.create" . mapM_ S.create . M.elems . syncsSyncs

-- | Perform action on model
withModel :: Syncs -> String -> (Sync -> a) -> a
withModel ss name f = maybe (error "No model with name") f $ M.lookup name (syncsSyncs ss)

-- | Insert Map into postgresql for model
insert :: Syncs -> String -> SyncMap -> TIO ()
insert ss name m = scope "Syncs.insert" $ withModel ss name $ \s -> S.insert s m

-- | Select row by condition
select :: Syncs -> String -> Condition -> TIO SyncMap
select ss name c = scope "Syncs.select" $ withModel ss name $ \s -> S.select s c

-- | Exists rows with condition
exists :: Syncs -> String -> Condition -> TIO Bool
exists ss name c = scope "Syncs.exists" $ withModel ss name $ \s -> S.exists s c

-- | Update by condition with values, stored in map
update :: Syncs -> String -> Condition -> SyncMap -> TIO ()
update ss name c m = scope "Syncs.update" $ withModel ss name $ \s -> S.update s c m

-- | Insert if not exists, update otherwise
-- Returns True on update
insertUpdate :: Syncs -> String -> Condition -> SyncMap -> TIO Bool
insertUpdate ss name c m = scope "Syncs.insertUpdate" $ withModel ss name $ \s -> S.insertUpdate s c m
