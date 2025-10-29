{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use underscore" #-}

module Streamly.External.Zip.Tests (tests) where

import Control.Concurrent.Async
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BC8
import Data.Function
import Data.Maybe
import Data.Time.Clock
import Data.Word
import GHC.Conc
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.External.Zip
import Streamly.External.Zip.Internal.Error
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Printf
import Text.Read

tests :: [TestTree]
tests =
  [ testDataZip,
    testDataZipFileInfo,
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

-- | Additional tests for @data.zip@: make sure the @FileInfo@s are as expected.
testDataZipFileInfo :: TestTree
testDataZipFileInfo = testCase "testDataZipFileInfo" $ do
  z <- openZip "test/data/data.zip" []

  -- Flip because in assertEqual, the expected value is the second one.
  let flipAssert :: (Eq a, Show a) => String -> String -> a -> a -> Assertion
      flipAssert fn field = flip $ assertEqual (printf "%s;%s" fn field)

  let checkFile ::
        String ->
        Maybe Int ->
        Maybe Int ->
        Maybe Int ->
        Maybe UTCTime ->
        Maybe Word32 ->
        Maybe CompressionMethod ->
        Maybe EncryptionMethod ->
        IO ()
      checkFile fn fidx fsz fcsz fmt fcrc fcm fem = do
        finfo <- getFileInfoAtPath z fn
        getFileName finfo >>= flipAssert fn "fn" (Just fn)
        getFileIndex finfo >>= flipAssert fn "fidx" fidx
        getFileSize finfo >>= flipAssert fn "fsz" fsz
        getFileCompressedSize finfo >>= flipAssert fn "fcsz" fcsz
        getFileModificationTime finfo >>= flipAssert fn "fmt" fmt
        getFileCRC finfo >>= flipAssert fn "fcrc" fcrc
        getFileCompressionMethod finfo >>= flipAssert fn "fcm" fcm
        getFileEncryptionMethod finfo >>= flipAssert fn "fem" fem

  -- Note: To obtain CRC-32 for a file ourselves, we can use the crc32 command (available via
  -- libarchive-zip-perl on Debian).

  checkFile
    "1byte"
    (Just 0)
    (Just 1)
    (Just 1)
    (Just . fromJust $ readMaybe @UTCTime "2022-10-08 11:30:08 UTC")
    (Just 0xdf6f85b3)
    (Just CM_STORE)
    (Just EM_NONE)

  checkFile
    "60kilobytes"
    (Just 1)
    (Just 60_000)
    (Just 60_000)
    (Just . fromJust $ readMaybe @UTCTime "2022-10-08 11:30:30 UTC")
    (Just 0x36b68602)
    (Just CM_STORE)
    (Just EM_NONE)

  checkFile
    "larger/"
    (Just 2)
    (Just 0)
    (Just 0)
    (Just . fromJust $ readMaybe @UTCTime "2022-10-08 11:31:26 UTC")
    (Just 0)
    (Just CM_STORE)
    (Just EM_NONE)

  checkFile
    "larger/1megabyte"
    (Just 3)
    (Just 1_000_000)
    (Just 1_000_000)
    (Just . fromJust $ readMaybe @UTCTime "2022-10-08 11:30:56 UTC")
    (Just 0xad8775ed)
    (Just CM_STORE)
    (Just EM_NONE)

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
