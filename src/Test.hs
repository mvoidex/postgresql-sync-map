{-# LANGUAGE OverloadedStrings #-}

module Test (
    test
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import Database.PostgreSQL.Sync

test :: Sync
test = sync [
    field "x"    "xrow"    int,
    field "name" "namerow" string]

testMap :: M.Map ByteString ByteString
testMap = M.fromList [
    ("x", "123"),
    ("name", "Vasya")]
