module Main where

import qualified Streamly.External.Zip.Tests
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup
        "Streamly.External.Zip.Tests"
        Streamly.External.Zip.Tests.tests
    ]