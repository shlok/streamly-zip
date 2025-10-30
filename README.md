# streamly-zip

[![Hackage](https://img.shields.io/hackage/v/streamly-zip.svg?style=flat)](https://hackage.haskell.org/package/streamly-zip)
[![CI](https://github.com/shlok/streamly-zip/actions/workflows/ci.yaml/badge.svg?branch=master)](https://github.com/shlok/streamly-zip/actions/workflows/ci.yaml)
[![built with garnix](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fshlok%2Fstreamly-zip%3Fbranch%3Dmaster)](https://garnix.io/repo/shlok/streamly-zip)

Stream data from zip archives using the Haskell [streamly](https://hackage.haskell.org/package/streamly) library.

## Comparison with streamly-archive

This library was created because libarchive (which [streamly-archive](https://hackage.haskell.org/package/streamly-archive) relies on) does not seem to have support for jumping to specific files even when the format supports it.

## Requirements

Install libzip on your system.

* Debian Linux: `sudo apt-get install libzip-dev`.
* macOS: `brew install libzip`.

## Quick start

```haskell
module Main where

import qualified Data.ByteString as B
import Data.Function
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.External.Zip

main :: IO ()
main = do
  -- Obtain an archive.
  z <- openZip "/path/to/archive.zip" []

  -- Output a particular file to stdout.
  S.unfold unfoldFileAtPath (z, [], "file.txt")
    & S.mapM B.putStr
    & S.fold F.drain
```
