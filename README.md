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
  ("age", "99"),
  ("flag", "some")]

-- | Create table
createA :: Connection -> IO ()
createA con = create con test

-- | Insert data with key (or Nothing for auto-key)
insertA :: Connection -> IO ()
insertA con = insert con test (Just 10) testData

-- | Select data with key
selectA :: Connection -> IO ()
selectA con = select con test 10

-- | Update data with key
updateA :: Connection -> IO ()
updateA con = update con test 10 testData
</pre>
