{-# LANGUAGE OverloadedStrings #-}

module Test (
    test, testMap
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import Database.PostgreSQL.Sync

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
