{-# LANGUAGE OverloadedStrings #-}

module Test (
    test, testMap
    ) where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import Data.List (intercalate)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Sync

test :: Sync
test = sync "test" "id" "garbage" [
    field "x"    "xrow"    int,
    field "name" "namerow" string]

testMap :: SyncMap
testMap = M.fromList [
    ("x", "123"),
    ("name", "Vasya"),
    ("blah", "foo"),
    ("quux", "lala")]

testMap2 :: SyncMap
testMap2 = M.fromList [
    ("x", "222"),
    ("blah", "fooo"),
    ("quuz", "alal")]

local :: ConnectInfo
local = ConnectInfo {
    connectHost = "localhost",
    connectPort = 5432,
    connectUser = "postgres",
    connectPassword = "declspec",
    connectDatabase = "postgres" }

elog :: IO () -> IO ()
elog act = E.catch act onError where
    onError :: E.SomeException -> IO ()
    onError e = putStrLn $ "Failed with: " ++ show e

data AnyField = AnyField { anyField :: ByteString }

instance FromField AnyField where
    fromField f Nothing = return $ AnyField (C8.pack "(null)")
    fromField f (Just s) = return $ AnyField s

run :: IO ()
run = do
    con <- connect local
    elog $ void $ execute_ con "CREATE EXTENSION hstore"
    elog $ void $ execute_ con "create table testa (id integer not null unique primary key, xrow integer, namerow text, garbage hstore)"
    commit con
    elog $ void $ execute_ con "drop table testa"
    elog $ void $ execute_ con "drop table test"
    transaction con $ create test
    transaction con $ do
        insert test (Just 1) testMap
        insert test (Just 2) testMap
        sm <- select test 1
        liftIO $ putStrLn $ "In 1: " ++ show sm
        update test 1 testMap2
        sm' <- select test 1
        liftIO $ putStrLn $ "In 1: " ++ show sm'
    anys <- query_ con "select * from test"
    mapM_ putStrLn $ map (intercalate " | " . map (C8.unpack . anyField)) $ anys
