module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Streamly.External.Zip.Tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Tests"
        [ testGroup "Streamly.External.Zip.Tests"
                     Streamly.External.Zip.Tests.tests ]
