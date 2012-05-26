sync-map
========

Synchronizing postgresql with Map

Declare fields mapping and then use 'store' and 'load' to convert data:

<pre>
test :: Sync
test = sync "table" "idcolumn" "garbage" [
  field "x"      "xrow"     int,     -- ^ 'x' will be stored in 'xrow' column with type int
  field "name"   "namerow"  string]  -- ^ 'name' will be stored in 'namerow' column with type string

testData = SyncMap
testData = M.fromList [
  ("x", "123"),
  ("name", "Vasya"),
  ("age", "99"),           -- ^ hstore
  ("flag", "some")]        -- ^ hstore

testUpdate = SyncMap
testUpdate = M.fromList [
  ("x", "222"),            -- ^ Update field
  ("flag", "some2"),       -- ^ Update field in hstore
  ("new", "111")]          -- ^ Add new field to hstore

-- | Create table
createA :: TIO ()
createA = create test

-- | Insert data with key (or Nothing for auto-key)
insertA :: Int -> SyncMap -> TIO ()
insertA i = insert test (Just i)

-- | Select data with key
selectA :: Int -> TIO SyncMap
selectA = select test

-- | Update data with key
updateA :: Int -> SyncMap -> TIO ()
updateA i = update test i

foo :: Connection -> IO ()
foo con = transaction con $ do
  createA
  insertA 1 testData      -- x=123, name=Vasya, {age=>99,flag=>some}
  updateA 1 testUpdate    -- x=222, name=Vasya, {age=>99,flag=>some2,new=>111}
  m <- selectA 1
  liftIO $ print m
</pre>
