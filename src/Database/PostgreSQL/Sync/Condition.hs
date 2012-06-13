module Database.PostgreSQL.Sync.Condition (
	toWhere, affects,
	condition, condField, syncsField, parseField,

	module Database.PostgreSQL.Sync.Base
	) where

import Control.Arrow
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Data.Either
import Database.PostgreSQL.Sync.Base
import Database.PostgreSQL.Sync.Types
import Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))

instance Monoid Condition where
    mempty = Condition [] "" []
    mappend (Condition [] "" []) r = r
    mappend l (Condition [] "" []) = l
    mappend (Condition tl sl al) (Condition tr sr ar) = Condition (nub $ tl ++ tr) (sl ++ " and " ++ sr) (al ++ ar)

toWhere :: Condition -> String
toWhere c
	| conditionString c == "" = ""
	| otherwise = " where " ++ conditionString c

-- | Check whether condition affects tables specified
affects :: [String] -> Condition -> Bool
affects tables cond = all (`elem` tables) $ conditionTablesAffected cond

-- | Create condition from string
condition :: Syncs -> String -> [Action] -> Condition
condition ss s args = Condition tables str args where
	tables = nub $ map fst $ lefts fields
	str = concatMap (either (\(t, n) -> t ++ "." ++ n) id) fields

	swords = words s
	fields = map (parseField' ss) swords
	parseField' :: Syncs -> String -> Either (String, String) String
	parseField' ss' s' = maybe (Right s') Left $ parseField ss' s'

condField :: Sync -> String -> (String, String)
condField (Sync t h cs) name = case find ((== name) . syncKey) cs of
    (Just (SyncField k c _)) -> (t, c)
    Nothing -> (t, h ++ " -> '" ++ C8.unpack (escapeHStore (C8.pack name)) ++ "'")

syncsField :: Syncs -> String -> String -> Maybe (String, String)
syncsField ss model name = fmap (\s -> condField s name) $ M.lookup model (syncsSyncs ss)

-- | Parse field "model.name" to table-related field "table.column" or "table.garbage -> 'name'"
parseField :: Syncs -> String -> Maybe (String, String)
parseField ss mname = syncsField ss model name where
	(model, name) = second (drop 1) $ break (== '.') mname
