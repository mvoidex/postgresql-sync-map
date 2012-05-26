{-# LANGUAGE OverloadedStrings #-}

module Test (
    test, testMap,
    run, runCmd
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
import Data.Function

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

runCmd :: IO ()
runCmd = do
    con <- connect local
    elog $ void $ execute_ con "create extension hstore"
    elog $ void $ execute_ con "drop table test"
    transaction con $ create test
    process [
        ("quit", stop),
        ("insert", takt $ do
            i <- intIO
            m <- dataIO
            transaction con $ insert test (Just i) m),
        ("select", takt $ do
            i <- intIO
            m <- transaction con $ select test i
            print m),
        ("update", takt $ do
            i <- intIO
            m <- dataIO
            transaction con $ update test i m)]
    where
        intIO :: IO Int
        intIO = putStrLn "index:" >> (getLine >>= readIO)
        dataIO :: IO SyncMap
        dataIO = fmap M.fromList (putStrLn "data:" >> (getLine >>= readIO))
        
        process ks = do
            k <- getLine
            case lookup k ks of
                Nothing -> do
                    putStrLn $ "Unknown command, possible commands are " ++ (intercalate ", " $ map fst ks)
                    process ks
                Just a -> do
                    b <- a
                    if b then process ks else return ()
        
        stop = return False
        takt act = elog act >> return True

run :: IO ()
run = do
    con <- connect local
    elog $ void $ execute_ con "CREATE EXTENSION hstore"
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
    --anys <- query_ con "select * from test"
    --mapM_ putStrLn $ map (intercalate " | " . map (C8.unpack . anyField)) $ anys
