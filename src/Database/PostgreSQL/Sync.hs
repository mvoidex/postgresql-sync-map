-- | To make convertor to and from specified map, use list of field connectors:
-- @
-- test = sync [
--   field "x"     "xrow"     int,     -- ^ Store 'x' as int in 'xrow'
--   field "y"     "yrow"     int,     -- ^ Store 'y' as int in 'yrow'
--   field "name"  "namerow"  string,  -- ^ Store 'name' as string in 'namerow'
--   other         "garbage"  hstore]  -- ^ Store all other fields as key-value container in 'garbage'
-- @
-- Now use it for reading-storing rows
-- TODO: Add example
module Database.PostgreSQL.Sync (
    Sync, SyncField,
    sync,
    field,
    store,
    load,
    
    Action,

    module Database.PostgreSQL.Sync.Types
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Sync.Types
import Data.List
import qualified Data.Map as M

-- | Sync connectors
data Sync = Sync {
    syncConnectors :: [SyncField] }
        deriving (Show)

-- | Field sync connector
data SyncField = SyncField {
    syncKey :: String,        -- ^ Key in Map
    syncColumn :: String,     -- ^ Column name in database
    syncType :: Type }        -- ^ Type of value

instance Show SyncField where
    show (SyncField k c (Type st _)) = unwords [k, "<->", c, "::", show st] -- TODO: Show type

-- | Make sync
sync :: [SyncField] -> Sync
sync = Sync

-- | Connect field
field :: String -> String -> Type -> SyncField
field = SyncField

-- | Store Map in postgresql
store :: Sync -> M.Map ByteString ByteString -> Either String (M.Map String Action)
store (Sync cs) = fmap (M.fromList) . mapM toAction . M.toList where
    toAction (k, v) = do
        (SyncField k' c' t') <- maybe (Left $ "Unable to find key " ++ C8.unpack k) return $ find ((== k) . C8.pack . syncKey) cs
        v' <- typeKey t' v
        return (c', v')

-- | Load Map from postgresql
load :: Sync -> M.Map String AsByteString -> Either String (M.Map ByteString ByteString)
load (Sync cs) = fmap (M.fromList) . mapM fromByteString . M.toList where
    fromByteString (c, f) = do
        (SyncField k' c' t') <- maybe (Left $ "Unable to find column " ++ c) return $ find ((== c) . syncColumn) cs
        f' <- fieldType t' f
        return (C8.pack k', f')
