{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Models (
    constant, adjustField,
    model, models,

    insert, select, exists, update, insertUpdate,

    module Database.PostgreSQL.Sync.Base,
    module Database.PostgreSQL.Sync.Types,
    module Database.PostgreSQL.Sync.Condition,
    module Database.PostgreSQL.Sync,
    module Database.PostgreSQL.Syncs
    ) where

import Prelude hiding (log)

import Control.Arrow
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Sync hiding (create, insert, select, exists, update, insertUpdate)
import Database.PostgreSQL.Sync.Base
import Database.PostgreSQL.Syncs hiding (insert, select, exists, update, insertUpdate)
import qualified Database.PostgreSQL.Sync as Sync
import qualified Database.PostgreSQL.Syncs as Syncs
import Database.PostgreSQL.Sync.Types
import Database.PostgreSQL.Sync.Condition
import Database.PostgreSQL.Simple.ToField
import qualified Data.Map as M
import System.Log

-- | Constant field constaint
constant :: String -> String -> ModelConstraint
constant name value = ModelConstraint {
    constraintFunction = (M.insert (C8.pack name) (C8.pack value), M.delete (C8.pack name)),
    constraintCondition = cond }
    where
        cond tbl = conditionSimple tbl name (\n -> n ++ " = ?") [toField value]

-- | Map some field
adjustField :: String -> (String -> String) -> (String -> String) -> ModelConstraint
adjustField name to from = ModelConstraint {
    constraintFunction = (M.adjust to' (C8.pack name), M.adjust from' (C8.pack name)),
    constraintCondition = const mempty }
    where
        to' = C8.pack . to . C8.unpack
        from' = C8.pack . from . C8.unpack

-- | Make model by sync
model :: String -> Sync -> [ModelConstraint] -> Model
model = Model

-- | Make models
models :: Syncs -> [(String, Model)] -> Models
models ss = Models ss . M.fromList

-- | Perform action on model
withModel :: Models -> String -> (Model -> a) -> a
withModel ms name f = maybe (error "No model with name") f $ M.lookup name (modelsModels ms)

instance Monoid ModelConstraint where
    mempty = ModelConstraint (id, id) mempty
    mappend (ModelConstraint (lf, lg) lc) (ModelConstraint (rf, rg) rc) = ModelConstraint (lf . rf, rg . lg) (lc `mappend` rc)

modelTo :: Model -> SyncMap -> SyncMap
modelTo = fst . constraintFunction . mconcat . modelConstraints

modelFrom :: Model -> SyncMap -> SyncMap
modelFrom = snd . constraintFunction . mconcat . modelConstraints

modelCondition :: Model -> Condition
modelCondition m = (constraintCondition . mconcat . modelConstraints $ m) (syncTable . modelSync $ m)

-- | Insert Map into postgresql for model
insert :: Models -> String -> SyncMap -> TIO ()
insert ms name m = scope "Models.insert" $ withModel ms name $ \mdl -> Sync.insert (modelSync mdl) (modelTo mdl m)

-- | Select row by condition
select :: Models -> String -> Condition -> TIO SyncMap
select ms name c = scope "Models.select" $ withModel ms name $ \mdl -> fmap (modelFrom mdl) (Sync.select (modelSync mdl) (c `mappend` modelCondition mdl))

-- | Exists rows with condition
exists :: Models -> String -> Condition -> TIO Bool
exists ms name c = scope "Models.exists" $ withModel ms name $ \mdl -> Sync.exists (modelSync mdl) (c `mappend` modelCondition mdl)

-- | Update by condition with values, stored in map
update :: Models -> String -> Condition -> SyncMap -> TIO ()
update ms name c m = scope "Models.update" $ withModel ms name $ \mdl -> Sync.update (modelSync mdl) (c `mappend` modelCondition mdl) (modelTo mdl m)

-- | Insert if not exists, update otherwise
-- Returns True on update
insertUpdate :: Models -> String -> Condition -> SyncMap -> TIO Bool
insertUpdate ms name c m = scope "Models.insertUpdate" $ withModel ms name $ \mdl -> Sync.insertUpdate (modelSync mdl) (c `mappend` modelCondition mdl) (modelTo mdl m)
