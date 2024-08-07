{-# LANGUAGE OverloadedStrings #-}

module Streamly.External.Zip.Tests (tests) where

import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BC8
import Data.Function
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.External.Zip
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [testDataZip]

testDataZip :: TestTree
testDataZip = testCase "testDataZip" $ do
  z <- openZip "test/data/data.zip" []

  numEntries <- getNumEntries z
  assertEqual "" numEntries 4

  paths <- forM [0 .. numEntries - 1] $ \idx -> getPathByIndex z idx []
  assertEqual "" paths ["1byte", "60kilobytes", "larger/", "larger/1megabyte"]
  let indexedPaths = zip [0 ..] paths

  let fol = S.fold (F.foldl' B.append B.empty)
      pathToBs p = S.unfold unfoldFileAtPath (z, [], p) & fol
      idxToBs idx = S.unfold unfoldFileAtIndex (z, [], idx) & fol

  fileBytestrings2 <- forM indexedPaths $ \(idx, path) -> do
    (,) <$> pathToBs (BC8.unpack path) <*> idxToBs idx

  let fileBytestrings = map fst fileBytestrings2
  assertEqual "" fileBytestrings (map snd fileBytestrings2)

  let hashes = map (Base16.encode . SHA256.hash) fileBytestrings
      expectedHashes =
        [ "2d3193691934124461809fb9bc7e671215099fc7d961bfbe31943d40d477c890",
          "8ef26f35dd26d5852a145e8ed64dad4db69820556fb267c23d76a513deaaaa80",
          -- SHA256 of empty string.
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          "b9a25f19d36f7e39ceff25e1463eeaa9800a8fae12551d46f826ae87ccba4a40"
        ]

  assertEqual "" hashes expectedHashes
