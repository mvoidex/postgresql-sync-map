{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

-- | To make convertor to and from specified map, use list of field connectors:
-- @
-- test = sync "table" "idcolumn" "garbage" [
--   field "x"     "xrow"     int,     -- ^ Store 'x' as int in 'xrow'
--   field "y"     "yrow"     int,     -- ^ Store 'y' as int in 'yrow'
--   field "name"  "namerow"  string]  -- ^ Store 'name' as string in 'namerow'
-- @
-- Now use it for reading-storing rows
-- @
-- -- Insert data
-- insert con test (Just 10) (M.fromList [("x", "123"), ("y", "22"), ("name", "Vasya"), ("blah", "blahblah"), ("foo", "bar")])
-- -- Select data by id
-- m <- select con test 10
-- -- Update specified fields (and also adds 'qoo' to hstore) by id
-- update con test 10 (M.fromList [("x", "333"), ("foo", "baz"), ("qoo", "aaa")]
-- @
module Database.PostgreSQL.Sync (
    Sync(..), SyncField(..),
    Condition(..), condField,
    sync,
    field,
    store,
    load,

    TIO, connection,
    create,
    insert, select, update,
    transaction,
    
    module Database.PostgreSQL.Sync.Types
    ) where

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Sync.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))
import qualified Database.PostgreSQL.Simple.ToRow as PG
import Data.List hiding (insert)
import Data.String
import qualified Data.Map as M

import qualified Debug.Trace as Debug

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

-- | Condition
data Condition = Condition {
    conditionString :: String,
    conditionArguments :: [Action] }
        deriving (Show)

instance Monoid Condition where
    mempty = Condition "" []
    mappend (Condition "" []) r = r
    mappend l (Condition "" []) = l
    mappend (Condition sl al) (Condition sr ar) = Condition (sl ++ " and " ++ sr) (al ++ ar)

condField :: Sync -> String -> String
condField (Sync t h cs) name = case find ((== name) . syncKey) cs of
    (Just (SyncField k c _)) -> t ++ "." ++ c
    Nothing -> t ++ "." ++ h ++ " -> '" ++ C8.unpack (escapeHStore (C8.pack name)) ++ "'"

instance Show SyncField where
    show (SyncField k c (Type st _ _)) = unwords [k, "<->", c, "::", show st]

-- | Make sync
sync :: String -> String -> [SyncField] -> Sync
sync = Sync

-- | Connect field
field :: String -> String -> Type -> SyncField
field k c t = SyncField k c t

-- | Store Map in postgresql
store :: Sync -> SyncMap -> Either String (M.Map String Action)
store (Sync tbl g cs) = fmap M.fromList . toActions . M.toList where
    toActions = showActions . partition hasColumn where
        showActions (cols, hstored) = do
            hstored' <- return $ toField (M.fromList hstored)
            cols' <- mapM toAction cols
            return ((g, hstored') : cols')
    hasColumn (k, _) = any ((== k) . C8.pack . syncKey) cs
    toAction (k, v) = do
        (SyncField k' c' t') <- maybe (Left $ "Unable to find key " ++ C8.unpack k) return $ find ((== k) . C8.pack . syncKey) cs
        v' <- valueToAction t' v
        return (c', v')

-- | Load Map from postgresql
load :: Sync -> M.Map String FieldValue -> Either String SyncMap
load (Sync tbl g cs) = fmap mconcat . mapM fromFieldValue . M.toList where
    fromFieldValue (c, v)
        | c == g = case v of
            HStoreValue m -> Right m
            _ -> Left "Invalid type, must be hstore"
        | otherwise = do
            (SyncField k' c' t') <- maybe (Left $ "Unable to find column " ++ c) return $ find ((== c) . syncColumn) cs
            if typeType t' /= valueType v
                then Left ("Type mismatching, need type " ++ show (typeType t') ++ ", got " ++ show (valueType v))
                else Right $ valueToSyncMap k' v

tt q a = Debug.traceShow q a

ttt q v a = do
    -- f <- formatQuery undefined q v
    Debug.traceShow q $ Debug.traceShow v a

newtype TIO a = TIO (ReaderT Connection IO a)
    deriving (Monad, Functor, Applicative, MonadIO)

-- | Get connection inside monad
connection :: TIO Connection
connection = TIO ask

-- | Create table if not exists
create :: Sync -> TIO ()
create s@(Sync tbl hs cons) = do
    con <- connection
    liftIO $ do
        putStrLn $ "has table " ++ tbl ++ "?"
        hasTable <- E.catch (tt qcheck (execute_ con qcheck) >> return True) (sqlError False)
        putStrLn $ if hasTable then "yes" else "no"
        unless hasTable $ do
            putStrLn "creating table"
            tt qcreate (execute_ con qcreate)
            putStrLn "commiting"
            commit con
    where
        qcheck = fromString $ "select 1 from " ++ tbl ++ " where 1 = 0"
        qcreate = fromString $ "create table " ++ tbl ++ " (" ++ intercalate ", " cols ++ ")"
        sqlError :: a -> E.SomeException -> IO a
        sqlError v _ = return v
        cols :: [String]
        cols = concat [
            map asType cons,
            [hs ++ " hstore"]]
        asType (SyncField _ c t) = unwords [c, typeCreateString t]

-- | Insert Map into postgresql
insert :: Sync -> SyncMap -> TIO ()
insert s@(Sync tbl _ _) m = connection >>= insert' where
    insert' con = liftIO $ void $ either onError onStore $ store s m where
        onStore m' = ttt q v $ execute con q v where
            q = fromString $ "insert into " ++ tbl ++ " (" ++ cols ++ ") values (" ++ qms ++ ")"
            cols = intercalate ", " $ M.keys m'
            qms = intercalate ", " $ replicate (M.size m') "?"
            v = M.elems m'
        onError str = error $ "Unable to insert data: " ++ str

-- | Select row by condition
select :: Sync -> Condition -> TIO SyncMap
select s@(Sync tbl g rs) (Condition w vs) = connection >>= select' where
    select' con = liftIO $ ttt q vs (query con q vs) >>= getHead >>= either onError return . load s . zipCols where
        q = fromString $ "select " ++ cols ++ " from " ++ tbl ++ " where " ++ w
        cols = intercalate ", " $ (map syncColumn rs ++ [g])
        zipCols = M.fromList . zip (map syncColumn rs ++ [g])
        getHead [x] = return x
        getHead xs = error $ "Expected one row, returned " ++ show (length xs) ++ " rows"
        onError str = error $ "Unable to convert data on select: " ++ str

-- | Update by condition with values, stored in map
update :: Sync -> Condition -> SyncMap -> TIO ()
update s@(Sync tbl g _) (Condition w vs) m = connection >>= update' where
    update' con = liftIO $ void $ either onError onUpdate $ store s m where
        onUpdate m' = ttt q v $ execute con q v where
            q = fromString $ "update " ++ tbl ++ " set " ++ cols ++ " where " ++ w
            cols = intercalate ", " $ map updater $ M.keys m'
            updater v
                -- FIXME: dirty
                | v == g = g ++ " = " ++ g ++ " || ?"
                | otherwise = v ++ " = ?"
            v = M.elems m' ++ vs
        onError str = error $ "Unable to update data: " ++ str

-- | Transaction
transaction :: Connection -> TIO a -> IO a
transaction con (TIO act) = runReaderT act con
--transaction con (TIO act) = withTransaction con (runReaderT act con)
