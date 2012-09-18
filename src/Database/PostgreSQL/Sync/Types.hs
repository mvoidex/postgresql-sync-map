{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Database.PostgreSQL.Sync.Types (
    SyncMap,
    FieldType(..), FieldValue(..),
    Type(..),
    typeType, valueType,
    valueToSyncMap,
    escapeHStore, escapeHStoreBS,
    escapeHKey, escapeHKeyBS,
    valueToAction,
    int, double, bool, string, time,
    
    Action
    ) where

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative
import Control.Arrow
import Data.Char
import Data.List
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Debug.Trace as Debug

type SyncMap = M.Map ByteString ByteString

data FieldType = IntType | DoubleType | BoolType | StringType | TimeType | HStoreType
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Value in field
data FieldValue = IntValue Int | DoubleValue Double | BoolValue Bool | StringValue String | TimeValue POSIXTime | HStoreValue SyncMap
    deriving (Eq, Ord, Show)

typeType :: Type -> FieldType
typeType t = typeField t

valueType :: FieldValue -> FieldType
valueType (IntValue _) = IntType
valueType (DoubleValue _) = DoubleType
valueType (BoolValue _) = BoolType
valueType (StringValue _) = StringType
valueType (TimeValue _) = TimeType
valueType (HStoreValue _) = HStoreType 

showToSyncMap :: Show a => String -> a -> SyncMap
showToSyncMap k v = M.singleton (fromString k) (fromString $ show v)

valueToSyncMap :: String -> FieldValue -> SyncMap
valueToSyncMap k (IntValue i) = showToSyncMap k i
valueToSyncMap k (DoubleValue i) = showToSyncMap k i
valueToSyncMap k (BoolValue i) = showToSyncMap k i
valueToSyncMap k (StringValue i) = showToSyncMap k i
valueToSyncMap k (TimeValue i) = showToSyncMap k (floor i :: Integer)
valueToSyncMap _ (HStoreValue i) = i

instance FromField SyncMap where
    fromField f Nothing = return M.empty
    fromField f (Just s) = if C8.null s then empty else parse s where
        trim = fst . C8.breakEnd (not . isSpace) . snd . C8.break (not . isSpace)
        trimq = removeq . trim where
            removeq s
                | C8.null s = s
                | C8.head s == '"' && C8.last s == '"' = C8.init . C8.tail $ s
                | otherwise = s
        -- | TODO: Escape!
        parse = 
            C8.split ',' >>>
            mapM (
                C8.breakSubstring (fromString "=>") >>>
                (trimq *** (fmap trimq . dropPrefix (fromString "=>"))) >>>
                check) >>>
            fmap M.fromList
            where
                dropPrefix s ss
                    | s `C8.isPrefixOf` ss = return $ C8.drop (C8.length s) ss
                    | otherwise = empty
                check (l, r) = fmap ((,) l) r
        -- parse = M.fromList . map (C8.breakSubstring (fromString "=>") >>> (trimq *** (trimq . C8.drop 2))) . C8.split ','

commonCh c = (isDigit c) || (isAlpha c) || (c `elem` "_")
-- commonCh c = not (isSpace c) && (c `notElem` "\"'")

-- | Escape HStore key or value in format 'key => value'
escapeHStore :: T.Text -> T.Text
escapeHStore b
    | T.null b = T.pack "\"\""
    | T.all commonCh b = b
    | otherwise = escaped b
--    | T.all (\c -> commonCh c || isSpace c || (c `elem` "\"'")) b = escaped b
--    | otherwise = error $ "HStore key or value has invalid value: " ++ T.unpack b

-- | Escape HStore key in column -> 'key' format
escapeHKey :: T.Text -> T.Text
escapeHKey b
    | T.null b = T.empty
    | T.all commonCh b = b
    | otherwise = escapedKey b

escapeHStoreBS :: ByteString -> ByteString
escapeHStoreBS = T.encodeUtf8 . escapeHStore . T.decodeUtf8

escapeHKeyBS :: ByteString -> ByteString
escapeHKeyBS = T.encodeUtf8 . escapeHKey . T.decodeUtf8

replaceWith :: Char -> String -> T.Text -> T.Text
replaceWith ch str = T.intercalate (T.pack str) . T.split (== ch)

escaped = T.cons '"' . (`T.snoc` '"') . replaceWith '"' "\\\"" . replaceWith '\'' "''"

escapedKey = replaceWith '\'' "''"

instance ToField SyncMap where
    toField = Many . plainQuote . intersperse (plain ", ") . map hstoredValue . M.toList where
        plain = Plain . fromByteString . fromString
        plainQuote = ([plain "'"] ++) . (++ [plain "'"])
        -- | TODO: Escape!
        hstoredValue (k, v) = Many [byteString k, plain "=>", byteString v] where
            byteString b = Plain (fromByteString $ escapeHStoreBS b)

data AnyValue = AnyValue { toAnyValue :: ByteString }
    deriving (Eq, Ord, Read, Show)

instance FromField AnyValue where
    fromField _ Nothing = return $ AnyValue C8.empty
    fromField _ (Just s) = return $ AnyValue s

instance FromField FieldValue where
    fromField f d = foldr1 (<|>) tries where
        tries = [
            (TimeValue . utcTimeToPOSIXSeconds) <$> fromField f d,
            IntValue <$> fromField f d,
            DoubleValue <$> fromField f d,
            BoolValue <$> fromField f d,
            StringValue <$> fromField f d,
            HStoreValue <$> fromField f d,
            (StringValue . C8.unpack . toAnyValue) <$> fromField f d]

-- | Type of column
data Type = Type {
    typeField :: FieldType,
    -- ^ No check of type, thx for type classes, we can't pass own RowParser for every field personally
    typeCreateString :: String,
    -- ^ Name for table create
    typeKey :: ByteString -> Either String Action }

tryRead :: Read a => ByteString -> Either String (Maybe a)
tryRead bs
    | C8.null bs = Right Nothing
    | C8.pack "null" == bs = Right Nothing
    | otherwise = case reads (C8.unpack bs) of
        [(v, s)] -> if all isSpace s then Right (Just v) else Left ("Can't read value: " ++ C8.unpack bs)
        _ -> Left ("Can't read value: " ++ C8.unpack bs)

-- | Convert string value to action
valueToAction :: Type -> ByteString -> Either String Action
valueToAction t s = typeKey t s

-- | Int type
int :: Type
int = Type IntType "integer" (fmap toField . (tryRead :: ByteString -> Either String (Maybe Int)))

-- | Double type
double :: Type
double = Type DoubleType "double precision" (fmap toField . (tryRead :: ByteString -> Either String (Maybe Double)))

-- | Bool type
bool :: Type
bool = Type BoolType "boolean" (fmap toField . tryReadBool) where
    tryReadBool "" = Right Nothing
    tryReadBool "null" = Right Nothing
    tryReadBool "True" = Right (Just True)
    tryReadBool "False" = Right (Just False)
    tryReadBool "true" = Right (Just True)
    tryReadBool "false" = Right (Just False)
    tryReadBool "1" = Right (Just True)
    tryReadBool "0" = Right (Just False)
    tryReadBool bs = Left ("Can't read value: " ++ C8.unpack bs)

-- | String type
string :: Type
string = Type StringType "text" (fmap toField . return)

-- | Time type
time :: Type
time = Type TimeType "timestamp" (fmap (toField . fmap (posixSecondsToUTCTime . (fromIntegral :: Int -> POSIXTime))) . (tryRead :: ByteString -> Either String (Maybe Int)))
