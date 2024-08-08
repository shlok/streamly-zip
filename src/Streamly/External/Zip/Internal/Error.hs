module Streamly.External.Zip.Internal.Error where

import Control.Exception
import qualified Data.ByteString.Char8 as BC
import Foreign.C.Types
import Foreign.Marshal
import Streamly.External.Zip.Internal.Foreign
import Text.Printf

data Error = Error !String !String

instance Show Error where
  show (Error ctx msg) = printf "streamly-zip; %s; %s" ctx msg

instance Exception Error

throwError :: String -> String -> m a
throwError ctx msg = throw $ Error ctx msg

libzipErrToString :: CInt -> IO String
libzipErrToString err = mask_ $ do
  errp <- mallocBytes zip_error_t_size
  c_zip_error_init_with_code errp err
  str <- BC.unpack <$> (c_zip_error_strerror errp >>= BC.packCString)
  c_zip_error_fini errp
  return str
