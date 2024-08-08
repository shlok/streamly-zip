module ReadmeMain where

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
