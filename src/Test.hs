{-# LANGUAGE OverloadedStrings #-}

module Test (
    test, testMap,
    run, runCmd
    ) where

import Control.Arrow
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import Data.List (intercalate)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Sync
import Database.PostgreSQL.Report
import Data.Function
import Data.String

test :: Sync
test = sync "test" "id" "garbage" ["blah", "quux", "quuz"] [
    field "x"    "xrow"    int,
    field "name" "namerow" string,
    reference "i" "irow" "test2"]

test2 :: Sync
test2 = sync "test2" "id" "garbage" ["ololo"] [
    field "car"  "car"     string,
    field "age"  "age"     int]

testMap :: SyncMap
testMap = M.fromList [
    ("x", "123"),
    ("name", "Vasya"),
    ("i", "1230"),
    ("blah", "foo"),
    ("quux", "lala")]

testMap2 :: SyncMap
testMap2 = M.fromList [
    ("x", "222"),
    ("name", "Frodo"),
    ("i", "2220"),
    ("blah", "fooo"),
    ("quuz", "alal")]

test2Map :: SyncMap
test2Map = M.fromList [
    ("car", "ford"),
    ("age", "1")]

test2Map2 :: SyncMap
test2Map2 = M.fromList [
    ("car", "bentley"),
    ("age", "3")]

local :: ConnectInfo
local = ConnectInfo {
    connectHost = "localhost",
    connectPort = 5432,
    connectUser = "postgres",
    connectPassword = "2741001",
    connectDatabase = "postgres" }

elog :: IO () -> IO ()
elog act = E.catch act onError where
    onError :: E.SomeException -> IO ()
    onError e = putStrLn $ "Failed with: " ++ show e

elogq :: (FromRow q) => IO [q] -> IO [q]
elogq act = E.catch act onError where
    onError :: (FromRow q) => E.SomeException -> IO [q]
    onError e = do
        putStrLn $ "Failed with: " ++ show e
        return []

data AnyValue = AnyValue { toAnyValue ::ByteString }
    deriving (Eq, Ord, Read, Show)

instance FromField AnyValue where
    fromField _ Nothing = return $ AnyValue C8.empty
    fromField _ (Just s) = return $ AnyValue s

runCmd :: IO ()
runCmd = do
    con <- connect local
    elog $ void $ execute_ con "create extension hstore"
    elog $ void $ execute_ con "drop table test"
    elog $ void $ execute_ con "drop table test2"
    transaction con $ create test
    transaction con $ create test2
    transaction con $ insert test (Just 0) testMap
    transaction con $ insert test (Just 1) testMap2
    transaction con $ insert test2 (Just 1230) test2Map
    transaction con $ insert test2 (Just 2220) test2Map2
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
            transaction con $ update test i m),
        ("execute", takt $ do
            q <- queryIO
            anys <- elogq $ query_ con (fromString q)
            mapM_ putStrLn $ map (intercalate " | " . map (C8.unpack . toAnyValue)) anys),
        ("report", takt $ do
            r <- reportIO
            rs <- transaction con $ generate [test, test2] r
            mapM_ putStrLn $ map (intercalate " | " . map show) rs)]
    where
        intIO :: IO Int
        intIO = putStrLn "index:" >> (getLine >>= readIO)
        dataIO :: IO SyncMap
        dataIO = fmap M.fromList (putStrLn "data:" >> (getLine >>= readIO))
        queryIO :: IO String
        queryIO = putStrLn "query:" >> getLine
        reportIO :: IO Report
        reportIO = putStrLn "report:" >> (getLine >>= fmap toReport . readIO) where
            toReport :: [(String, String)] -> Report
            toReport = map (second fromCondition) >>> Report
            fromCondition "*" = anyValue
            fromCondition s = equal s
        
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
