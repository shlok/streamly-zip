{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streamly.External.Zip.Tests (tests) where

import Control.Concurrent.Async
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BC8
import Data.Function
import GHC.Conc
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.External.Zip
import Streamly.External.Zip.Internal.Error
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testDataZip,
    testDataZipOneChunkConcurrent
  ]

-- | Reads all items from @data.zip@ and makes sure they are as expected.
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

-- |
-- * Reads at most one chunk from each item in @data.zip@, numerous times concurrently.
-- * Original motivation: reproduce unpredictable segfaults in version 0.0.1. We’re not fully
--   certain we succeeded with the exact segfault we encountered (occurring for the same reasons),
--   but we did produce some runtime crashes: at least @free(): invalid pointer@, @double free or
--   corruption (fasttop),@ @double free or corruption (out)@, and silent runtime crashes. (Maybe
--   those silent ones were @cabal test@ catching segfaults; see
--   https://github.com/UnkindPartition/tasty/issues/354 for potential clues.)
testDataZipOneChunkConcurrent :: TestTree
testDataZipOneChunkConcurrent = testProperty "testDataZipOneChunkConcurrent" $ monadicIO $ do
  assertEnoughCapabilities
  numConcurrent <- pick $ chooseInt (1, 2 * numCapabilities)
  run $ replicateConcurrently_ numConcurrent $ do
    numEntries <- openZip "test/data/data.zip" [] >>= getNumEntries
    forM_ [0 .. numEntries - 1] $ \idx -> do
      z <- openZip "test/data/data.zip" []
      _ <-
        S.unfold @IO unfoldFileAtIndex (z, [], idx)
          & S.fold F.one
      return ()

assertEnoughCapabilities :: (Monad m) => PropertyM m ()
assertEnoughCapabilities =
  when
    (numCapabilities <= 1)
    ( run $
        throwError
          "assertEnoughCapabilities"
          "available threads <= 1; machine / Haskell RTS settings not useful for testing"
    )
