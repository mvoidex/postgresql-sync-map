module Database.PostgreSQL.Sync.Condition (
	toWhere, affects,
	conditionSimple, conditionComplex,
	FieldName,
    condField, syncsField, modelsField, splitField, catField, parseField,
    convertField, convertSyncs, convertModels,

	module Database.PostgreSQL.Sync.Base
	) where

import Control.Arrow
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Data.Char
import Data.Either
import qualified Data.Text as T
import Database.PostgreSQL.Sync.Base
import Database.PostgreSQL.Sync.Types
import Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))

instance Monoid Condition where
    mempty = Condition [] [] "" []
    mappend (Condition [] [] "" []) r = r
    mappend l (Condition [] [] "" []) = l
    mappend (Condition tl fl sl al) (Condition tr fr sr ar) = Condition
        (nub $ tl ++ tr)
        (nub $ fl ++ fr)
        (sl ++ " and " ++ sr)
        (al ++ ar)

toWhere :: Condition -> String
toWhere c
	| conditionString c == "" = ""
	| otherwise = " where " ++ conditionString c

-- | Check whether condition affects tables specified
affects :: [String] -> Condition -> Bool
affects tables cond = all (`elem` tables) $ conditionTablesAffected cond

-- | Create condition on one field and table
conditionSimple :: String -> String -> (String -> String) -> [Action] -> Condition
conditionSimple table field fcond acts = Condition [table] [field] (fcond (table ++ "." ++ field)) acts

-- | Create condition from string
conditionComplex :: Syncs -> String -> [Action] -> Condition
conditionComplex ss s args = Condition tables fields' str args where
	tables = nub $ map fst $ lefts fields
	fields' = nub $ map (\(t, n) -> t ++ "." ++ n) $ lefts fields
	-- TODO: Rewrite!
	str = "(" ++ (unwords $ map (either (\(t, n) -> t ++ "." ++ n) id) fields) ++ ")"

	swords = words s
	fields = map (parseField' ss) swords
	parseField' :: Syncs -> String -> Either (String, String) String
	parseField' ss' s' = maybe (Right s') Left $ parseField ss' s'

type FieldName = (String, String)

condField :: Sync -> String -> FieldName
condField (Sync t h cs) name = case find ((== name) . syncKey) cs of
    (Just (SyncField k c _ _)) -> (t, c)
    Nothing -> (t, h ++ " -> '" ++ T.unpack (escapeHKey (T.pack name)) ++ "'")

-- | Convert (model, name) to (table, field) by Syncs
syncsField :: Syncs -> String -> String -> Maybe FieldName
syncsField ss model name = fmap (\s -> condField s name) $ M.lookup model (syncsSyncs ss)

-- | Convert (model, name) to (table, field) by Models
modelsField :: Models -> String -> String -> Maybe FieldName
modelsField ms model name = fmap (\s -> condField (modelSync s) name) $ M.lookup model (modelsModels ms)

-- | Split model.name to (model, name)
splitField :: String -> Maybe FieldName
splitField str = if valid then Just (model, name) else Nothing where
	valid = all (\c -> isAlpha c || isDigit c || c `elem` "._") str
	(model, name) = second (drop 1) $ break (== '.') str

-- | Concat (table, field) to table.field
catField :: FieldName -> String
catField (model, name) = model ++ "." ++ name

-- | Parse field "model.name" to table-related field "table.column" or "table.garbage -> 'name'"
parseField :: Syncs -> String -> Maybe FieldName
parseField ss mname = if valid then syncsField ss model name else Nothing where
	(model, name) = second (drop 1) $ break (== '.') mname
	valid = all (\c -> isAlpha c || isDigit c || c `elem` "._") name

-- | Convert
convertField :: (a -> FieldName -> Maybe FieldName) -> a -> String -> Maybe String
convertField f v = splitField >=> f v >=> (return . catField)

-- | Convert by Syncs
convertSyncs :: Syncs -> String -> Maybe String
convertSyncs = convertField (\v -> uncurry (syncsField v))

-- | Convert by Models
convertModels :: Models -> String -> Maybe String
convertModels = convertField (\v -> uncurry (modelsField v))
