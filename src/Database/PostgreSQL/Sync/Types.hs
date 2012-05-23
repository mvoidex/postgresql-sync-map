module Database.PostgreSQL.Sync.Types (
    SourceType(..),
    AsByteString(..),
    Type(..), fieldType,
    int, double, bool, string,
    
    Action
    ) where

import Control.Applicative
import Data.Char
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

-- | Source type
data SourceType = IntColumn | DoubleColumn | BoolColumn | StringColumn
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

-- | Convertible to ByteString
data AsByteString = AsByteString {
    asSourceType :: SourceType,
    asByteString :: ByteString }
        deriving (Eq, Ord, Read, Show)

instance FromField AsByteString where
    fromField f d = foldr1 (<|>) tries where
        tries = [
            (showBS IntColumn :: Int -> AsByteString) <$> fromField f d,
            (showBS DoubleColumn :: Double -> AsByteString) <$> fromField f d,
            (showBS BoolColumn :: Bool -> AsByteString) <$> fromField f d,
            (AsByteString StringColumn) <$> fromField f d]
        showBS :: (Show a) => SourceType -> a -> AsByteString
        showBS t = AsByteString t . C8.pack . show

-- | Type of column
data Type = Type {
    typeField :: SourceType,
    -- ^ No check of type, thx for type classes, we can't pass own RowParser for every field personally
    typeKey :: ByteString -> Either String Action }

tryRead :: Read a => ByteString -> Either String a
tryRead bs = case reads (C8.unpack bs) of
    [(v, s)] -> if all isSpace s then Right v else Left "Can't read value"
    _ -> Left "Can't read value"

sourceType :: SourceType -> AsByteString -> Either String ByteString
sourceType st (AsByteString st' ss')
    | st == st' = Right ss'
    | otherwise = Left "Invalid type of field"

fieldType :: Type -> AsByteString -> Either String ByteString
fieldType (Type tf _) = sourceType tf

-- | Int type
int :: Type
int = Type IntColumn (fmap toField . (tryRead :: ByteString -> Either String Int))

-- | Double type
double :: Type
double = Type DoubleColumn (fmap toField . (tryRead :: ByteString -> Either String Double))

-- | Bool type
bool :: Type
bool = Type BoolColumn (fmap toField . (tryRead :: ByteString -> Either String Bool))

-- | String type
string :: Type
string = Type StringColumn (fmap toField . return)
