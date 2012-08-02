{-# LANGUAGE OverloadedStrings #-}

-- | Provides loading Sync and Syncs from JSON
module Database.PostgreSQL.Sync.JSON (
    ) where

import Control.Applicative
import Data.Aeson
import qualified Data.Map as M

import Database.PostgreSQL.Sync.Base
import Database.PostgreSQL.Sync.Types
import Database.PostgreSQL.Syncs

instance FromJSON Type where
    parseJSON (String "int") = return int
    parseJSON (String "double") = return double
    parseJSON (String "bool") = return bool
    parseJSON (String "string") = return string
    parseJSON (String "time") = return time
    parseJSON _ = empty

instance FromJSON SyncField where
    parseJSON (Object v) = do
        nm <- v .: "name"
        indexed <- (v .: "indexed") <|> pure False
        tp <- v.: "type"
        return $ SyncField nm nm indexed tp
    parseJSON _ = empty

instance FromJSON Sync where
    parseJSON (Object v) = Sync <$> (v .: "table") <*> (v .: "hstore") <*> (v .: "fields")
    parseJSON _ = empty

data InSync = InSync {
    inSync :: (String, Sync) }

instance FromJSON InSync where
    parseJSON obj@(Object v) = InSync <$> ((,) <$> (v .: "model") <*> (parseJSON obj))
    parseJSON _ = empty

instance FromJSON Syncs where
    parseJSON (Object v) = do
        ss <- fmap (map inSync) $ v .: "syncs"
        cs <- v .: "relations"
        return $ syncs ss cs
    parseJSON _ = empty
