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
    sync,
    field, field_, indexed,
    store,
    load,

    TIO, connection,
    create,
    insert, select, exists, update, insertUpdate,
    transaction, inPG,
    
    module Database.PostgreSQL.Sync.Base,
    module Database.PostgreSQL.Sync.Types,
    module Database.PostgreSQL.Sync.Condition
    ) where

import Prelude hiding (log, catch)

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.CatchIO
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Sync.Base
import Database.PostgreSQL.Sync.Types
import Database.PostgreSQL.Sync.Condition
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))
import qualified Database.PostgreSQL.Simple.ToRow as PG
import Data.List hiding (insert)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import System.Log

-- | Make sync
sync :: String -> String -> [SyncField] -> Sync
sync = Sync

-- | Connect field
field :: String -> String -> Type -> SyncField
field k c t = SyncField k c False t

-- | Connect field without renaming
field_ :: String -> Type -> SyncField
field_ k = field k k

-- | Make field indexed
indexed :: SyncField -> SyncField
indexed f = f { syncIndexed = True }

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
        (SyncField k' c' _ t') <- maybe (Left $ "Unable to find key " ++ C8.unpack k) return $ find ((== k) . C8.pack . syncKey) cs
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
            (SyncField k' c' _ t') <- maybe (Left $ "Unable to find column " ++ c) return $ find ((== c) . syncColumn) cs
            if typeType t' /= valueType v
                then Left ("Type mismatching, need type " ++ show (typeType t') ++ ", got " ++ show (valueType v))
                else Right $ valueToSyncMap k' v

elog :: (MonadCatchIO m, MonadLog m) => m () -> m ()
elog act = catch act onError where
    onError :: (MonadCatchIO m, MonadLog m) => E.SomeException -> m ()
    onError e = log Error $ fromString $ "Ignoring exception: " ++ show e

elogt :: (MonadCatchIO m, MonadLog m) => m a -> m a
elogt act = catch act onError where
    onError :: (MonadCatchIO m, MonadLog m) => E.SomeException -> m a
    onError e = do
        log Error $ fromString $ "Rethrowing exception: " ++ show e
        throw e

data SyncConnection = SyncConnection {
    syncConn :: Connection,
    syncLog :: Log }

newtype TIO a = TIO (ReaderT SyncConnection IO a)
    deriving (Monad, Functor, Applicative, MonadIO, MonadCatchIO)

-- | Get connection inside monad
connection :: TIO Connection
connection = TIO $ asks syncConn

instance MonadLog TIO where
    askLog = TIO $ asks syncLog

-- | Create table if not exists
create :: Sync -> TIO ()
create s@(Sync tbl hs cons) = scope "Sync.create" $ do
    con <- connection
    log Trace $ fromString $ "Creating sync for table " ++ tbl
    log Debug $ fromString $ "Checking whether database has table " ++ tbl
    log Trace $ fromString $ "Query: " ++ qcheck
    hasTable <- liftIO $ catch (execute_ con (fromString qcheck) >> return True) (sqlError False)
    unless hasTable $ elog $ do
        log Debug $ fromString $ "Table " ++ tbl ++ " doesn't exist"
        log Trace $ fromString $ "Creating table " ++ tbl
        log Trace $ fromString $ "Create table query: " ++ qcreate
        liftIO $ execute_ con (fromString qcreate)
        log Trace $ fromString $ "Creating indexes for table " ++ tbl
        forM_ cons (createIndex con)
        liftIO $ commit con
    
    where
        qcheck = "select 1 from " ++ tbl ++ " where 1 = 0"
        qcreate = "create table " ++ tbl ++ " (" ++ intercalate ", " cols ++ ")"
        sqlError :: a -> E.SomeException -> IO a
        sqlError v _ = return v
        cols :: [String]
        cols = concat [
            map asType cons,
            [hs ++ " hstore"]]
        asType (SyncField _ c _ t) = unwords [c, typeCreateString t]
        createIndex _ (SyncField _ c False _) = return ()
        createIndex con (SyncField _ c True _) = elog $ void $ do
            log Trace $ fromString $ "Create index query: " ++ qindex
            liftIO $ execute_ con (fromString qindex)
            where
                qindex = fromString $ "create index " ++ c ++ "_index on " ++ tbl ++ " (" ++ c ++ ")"

-- | Insert Map into postgresql
insert :: Sync -> SyncMap -> TIO ()
insert s@(Sync tbl _ _) m = scope "Sync.insert" $ do
    con <- connection
    void $ insert' con
    where
        insert' con = void $ either onError onStore $ store s m where
            onStore m' = do
                log Trace $ fromString $ "Inserting data into " ++ tbl
                fmtQ <- liftIO $ formatQuery con q v
                log Trace $ T.concat ["Insert query: ", T.decodeUtf8 fmtQ]
                liftIO $ execute con q v
                where
                    q = fromString $ "insert into " ++ tbl ++ " (" ++ cols ++ ") values (" ++ qms ++ ")"
                    cols = intercalate ", " $ M.keys m'
                    qms = intercalate ", " $ replicate (M.size m') "?"
                    v = M.elems m'
            onError str = error $ "Unable to insert data: " ++ str
            
-- | Select row by condition
select :: Sync -> Condition -> TIO SyncMap
select s@(Sync tbl g rs) cond = scoper "Sync.select" $ do
    con <- connection
    select' con
    where
        select' con = do
            log Trace $ fromString $ "Selecting data from " ++ tbl
            fmtQ <- liftIO $ formatQuery con q (conditionArguments cond)
            log Trace $ T.concat ["Select query: ", T.decodeUtf8 fmtQ]
            liftIO $ query con q (conditionArguments cond) >>= getHead >>= either onError return . load s . zipCols
            where
                q = fromString $ "select " ++ cols ++ " from " ++ tbl ++ toWhere cond
                cols = intercalate ", " $ (map syncColumn rs ++ [g])
                zipCols = M.fromList . zip (map syncColumn rs ++ [g])
                getHead [x] = return x
                getHead xs = error $ "Expected one row, returned " ++ show (length xs) ++ " rows"
                onError str = error $ "Unable to convert data on select: " ++ str                

-- | Exists rows with condition
exists :: Sync -> Condition -> TIO Bool
exists s@(Sync tbl g rs) cond = scoper "Sync.exists" $ do
    con <- connection
    exists' con
    where
        exists' con = do
            fmtQ <- liftIO $ formatQuery con q (conditionArguments cond)
            log Trace $ T.concat ["Exists query: ", T.decodeUtf8 fmtQ]
            liftIO $ query con q (conditionArguments cond) >>= getHead >>= return . fromOnly
            where
                q = fromString $ "select count(*) > 0 from " ++ tbl ++ toWhere cond
                getHead [x] = return x
                getHead xs = error $ "Impossible happened, count must return one row"
    
-- | Update by condition with values, stored in map
update :: Sync -> Condition -> SyncMap -> TIO ()
update s@(Sync tbl g _) cond m = scope "Sync.update" $ do
    con <- connection
    update' con
    where
        update' con = void $ either onError onUpdate $ store s m where
            onUpdate m' = do
                fmtQ <- liftIO $ formatQuery con q v
                log Trace $ T.concat ["Update query: ", T.decodeUtf8 fmtQ]
                liftIO $ execute con q v
                where
                    q = fromString $ "update " ++ tbl ++ " set " ++ cols ++ toWhere cond
                    cols = intercalate ", " $ map updater $ M.keys m'
                    updater vv
                        -- FIXME: dirty
                        | vv == g = g ++ " = " ++ g ++ " || ?"
                        | otherwise = vv ++ " = ?"
                    v = M.elems m' ++ conditionArguments cond
            onError str = error $ "Unable to update data: " ++ str

-- | Insert if not exists, update otherwise
-- Returns True on update
insertUpdate :: Sync -> Condition -> SyncMap -> TIO Bool
insertUpdate s cond m = scope "Sync.insertUpdate" $ do
    e <- exists s cond
    log Trace $ fromString $ if e then "Updating existing data" else "Inserting new data"
    if e then update s cond m else insert s m
    return (not e)

-- | Transaction
transaction :: Connection -> TIO a -> ReaderT Log IO a
transaction con (TIO act) = do
    l <- askLog
    liftIO $ runReaderT act (SyncConnection con l)
--transaction con (TIO act) = withTransaction con (runReaderT act con)

-- | For now there is no module for use with snaplet-postgresql-simple,
-- but you can use functions using this simple wrap:
-- withPG (inPG $ update ...)
inPG :: TIO a -> Connection -> ReaderT Log IO a
inPG = flip transaction
