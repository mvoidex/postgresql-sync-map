{-# LANGUAGE OverloadedStrings #-}

module Test (
    test, testMap,
    run
    ) where

import Control.Arrow
import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Char
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Syncs as S
import qualified Database.PostgreSQL.Models as SM
import Database.PostgreSQL.Report
import Database.PostgreSQL.Report.Xlsx
import Data.Maybe
import Data.Function
import Data.String
import qualified Data.Text as T
import System.FilePath
import System.Directory
import System.Log

import Debug.Trace

test :: Sync
test = sync "test" "garbage" [
    field "id"   "id"      int,
    field "x"    "xrow"    int,
    field "name" "namerow" string,
    field "i" "irow" string]
    -- reference "i" "irow" "test2"]

test2 :: Sync
test2 = sync "test2" "garbage" [
    field "id"   "id"      int,
    field "car"  "car"     string,
    field "age"  "age"     int]

caseModel :: Sync
caseModel = S.sync "casetbl" "garbage" [
    S.indexed $ S.field_ "id" S.int,
    S.indexed $ S.field_ "car_make" S.string,
    S.indexed $ S.field_ "car_model" S.string,
    S.indexed $ S.field_ "car_program" S.string,
    S.indexed $ S.field_ "car_vin" S.string,
    S.field_ "car_buyDate" S.time,
    S.field_ "car_plateNum" S.string,
    S.field_ "car_carModel" S.string,
    S.indexed $ S.field_ "diagnosis1" S.string,
    S.indexed $ S.field_ "diagnosis2" S.string,
    S.indexed $ S.field_ "dealerCause" S.string,
    S.field_ "caseAddress_address" S.string,
    S.indexed $ S.field_ "callDate" S.time,
    S.field_ "callTaker" S.string,
    S.field_ "callerOwner" S.int,
    S.field_ "caller_name" S.string,
    S.indexed $ S.field_ "comment" S.string,
    S.field_ "program" S.string,
    S.field_ "services" S.string,
    S.field_ "owner_name" S.string,
    S.field_ "partner_name" S.string]

serviceModel :: Sync
serviceModel = S.sync "servicetbl" "garbage" [
    S.field_ "id" S.int,
    S.field_ "parentId" S.int,
    S.field_ "status" S.string,
    S.field_ "type" S.string,
    S.field_ "falseCall" S.string,
    S.field_ "towDealer_name" S.string,
    S.field_ "orderNumber" S.int,
    S.field_ "suburbanMilage" S.string,
    S.field_ "warrantyCase" S.string,
    S.field_ "times_repairEndDate" S.time,
    S.field_ "times_factServiceStart" S.time,
    S.field_ "times_factServiceEnd" S.time,
    S.field_ "carProvidedFor" S.int,
    S.field_ "hotelProvidedFor" S.int,
    S.field_ "payment_limitedCost" S.int,
    S.field_ "payment_partnerCost" S.int,
    S.field_ "payType" S.string,
    S.field_ "clientSatisfied" S.string]

service :: String -> (String, SM.Model)
service tp = (tp, mdl) where
    mdl = SM.model tp serviceModel [
        SM.constant "type" tp,
        SM.adjustField "parentId" (drop 5) ("case:" ++)]

tests :: Syncs
tests = syncs [
    ("case", caseModel),
    ("service", serviceModel)] [
    "case.id = service.parentId"]

models :: SM.Models
models = SM.models tests $ [
    ("case", SM.model "case" caseModel [])] ++ map service [
    "deliverCar",
    "deliverParts",
    "hotel",
    "information",
    "rent",
    "sober",
    "taxi",
    "tech",
    "towage",
    "transportation"]

testMap :: SyncMap
testMap = M.fromList [
    ("id", "0"),
    ("x", "123"),
    ("name", "Vasya"),
    ("i", "123"),
    ("blah", "foo"),
    ("quux", "lala")]

testMap2 :: SyncMap
testMap2 = M.fromList [
    ("id", "1"),
    ("x", "222"),
    ("name", "Frodo"),
    ("i", "222"),
    ("blah", "fooo"),
    ("quuz", "alal")]

test2Map :: SyncMap
test2Map = M.fromList [
    ("id", "123"),
    ("car", "ford"),
    ("age", "1")]

test2Map2 :: SyncMap
test2Map2 = M.fromList [
    ("id", "222"),
    ("car", "bentley"),
    ("age", "3")]

local :: ConnectInfo
local = ConnectInfo {
    connectHost = "localhost",
    connectPort = 5432,
    connectUser = "postgres",
    connectPassword = "pass",
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

elogi :: (Num a) => IO a -> IO a
elogi act = E.catch act onError where
    onError :: (Num b) => E.SomeException -> IO b
    onError e = do
        putStrLn $ "Failed with " ++ show e
        return 0

data AnyValue = AnyValue { toAnyValue :: ByteString }
    deriving (Eq, Ord, Read, Show)

instance FromField AnyValue where
    fromField _ Nothing = return $ AnyValue C8.empty
    fromField _ (Just s) = return $ AnyValue s

data KeyValue = KeyValue {
    key :: T.Text,
    value :: T.Text }
        deriving (Show)

data Dictionary = Dictionary {
    entries :: [KeyValue] }
        deriving (Show)

instance FromJSON KeyValue where
    parseJSON (Object v) = KeyValue <$> (v .: (T.pack "value")) <*> (v .: (T.pack "label"))

instance FromJSON Dictionary where
    parseJSON (Object v) = Dictionary <$> (v .: (T.pack "entries"))

dictionary :: Dictionary -> M.Map String String
dictionary = M.fromList . map ((T.unpack . key) &&& (T.unpack . value)) . entries

loadMap :: FilePath -> IO (M.Map String String)
loadMap = fmap (maybe M.empty dictionary) . loadDictionary

loadMaps :: FilePath -> [String] -> IO (M.Map String (M.Map String String))
loadMaps f ds = fmap M.fromList $ forM ds $ \d -> do
    m <- loadMap (f </> (d ++ ".json"))
    return (d, m)

loadDictionary :: FilePath -> IO (Maybe Dictionary)
loadDictionary f = fmap decode $ LC8.readFile f

loadValue :: FilePath -> IO (Maybe Value)
loadValue f = fmap decode $ LC8.readFile f

loadDicts :: FilePath -> IO (M.Map String (M.Map String String))
loadDicts cfg = do
    files <- getDirectoryContents cfg
    let dictNames = map (dropExtension . takeFileName) $ filter ((== ".json") . takeExtension) files
    loadMaps cfg dictNames

functions :: M.Map String (M.Map String String) -> [ReportFunction]
functions ds = [
    uses ["case.id"] $ constFunction "CASEID" (M.lookup "case.id"),
    onField "ID" id,
    onString "NAME" (fromMaybe "" . listToMaybe . drop 1 . words),
    onString "SURNAME" (fromMaybe "" . listToMaybe . words),
    onString "UPPER" (map toUpper),
    onString "LOWER" (map toLower),
    function "CONCAT" concatFields,
    functionMaybe "LOOKUP" lookupField]
    where
        concatFields :: [FieldValue] -> FieldValue
        concatFields fs = StringValue $ concat $ mapMaybe fromStringField fs
        fromStringField (StringValue s) = Just s
        fromStringField _ = Nothing

        lookupField :: [FieldValue] -> Maybe FieldValue
        lookupField [StringValue s, StringValue d] = do
            d' <- M.lookup d ds
            s' <- M.lookup s d'
            return $ StringValue s'
        lookupField _ = Nothing

run :: IO ()
run = do
    dicts <- loadDicts "/home/voidex/Documents/Projects/carma/srv/resources/site-config/dictionaries"
    con <- connect local
    elog $ void $ execute_ con "create extension hstore"
    elog $ void $ withNoLog $ transaction con $ create tests
    process [
        ("quit", stop),
        ("drop", takt $ do
            tbl <- getLine
            elog $ void $ execute_ con (fromString $ "drop table " ++ tbl)),
        ("insert", takt $ do
            w <- modelIO
            m <- dataIO
            withNoLog $ transaction con $ SM.insert models w m),
        ("select", takt $ do
            w <- modelIO
            i <- intIO
            -- TODO: condition is not good function
            m <- withNoLog $ transaction con $ SM.select models w (conditionComplex tests (w ++ ".id = ?") [toField i])
            print m),
        ("update", takt $ do
            w <- modelIO
            i <- intIO
            m <- dataIO
            withNoLog $ transaction con $ SM.update models w (conditionComplex tests (w ++ ".id = ?") [toField i]) m),
        ("execute", takt $ do
            q <- queryIO
            anys <- elogq $ query_ con (fromString q)
            mapM_ putStrLn $ map (intercalate " | " . map (C8.unpack . toAnyValue)) anys),
        ("execute_", takt $ do
            q <- queryIO
            i <- elogi $ execute_ con (fromString q)
            putStrLn $ "rows affected: " ++ show i),
        ("report", takt $ do
            r <- reportIO
            rs <- withNoLog $ transaction con $ generate r tests (functions dicts)
            mapM_ putStrLn $ map (intercalate " | " . map show) rs),
        ("run-report", takt $ do
            f <- getLine
            t <- getLine
            c <- getLine >>= readIO
            withNoLog $ transaction con $ createReport tests (functions dicts) (const []) c [] f t)]
    where
        modelIO :: IO String
        modelIO = putStrLn "model:" >> getLine
        intIO :: IO Int
        intIO = putStrLn "index:" >> (getLine >>= readIO)
        dataIO :: IO SyncMap
        dataIO = fmap M.fromList (putStrLn "data:" >> (getLine >>= readIO))
        queryIO :: IO String
        queryIO = putStrLn "query:" >> getLine
        reportIO :: IO Report
        reportIO = do
            putStrLn "(field-with-condition)s:"
            fs <- getLine >>= readIO
            return $ fromMaybe (error "Unable to create report") $ report fs
        
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
