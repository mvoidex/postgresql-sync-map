sync-map
========

Synchronizing postgresql with Map

Declare fields mapping and then use 'store' and 'load' to convert data:

<pre>
test :: Sync
test = sync [
  field "x"      "xrow"     int,     -- ^ 'x' will be stored in 'xrow' column with type int
  field "name"   "namerow"  string]  -- ^ 'name' will be stored in 'namerow' column with type string
  blob "garbage"                     -- ^ other data will be stored in hstore column 'garbage'

testData :: M.Map ByteString ByteString
testData = M.fromList [
  ("x", "123"),
  ("name", "Vasya"),
  ("age", "99"),
  ("flag", "some")]

-- | To store data, convert Map with function 'store' to list of Action, then use this list in query
-- (UPDATE or INSERT)
testStore :: Either String (M.Map String Action)
testStore = store test testData
-- ^ Right (fromList [
--     ("xrow", Plain "123"),
--     ("namerow", Escape "Vasya"),
--     undefined] -- not implemented yet

-- | To load data, read list of [AsByteString], then convert it to Map with function 'load'
-- Note, that AsByteString tries to load any possible value, converts it to ByteString, but
-- saves information about type to perform check in 'load'.
testLoad :: [AsByteString] -> Either String (M.Map ByteString ByteString)
testLoad = load test 
</pre>
