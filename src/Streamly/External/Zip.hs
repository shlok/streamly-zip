{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The documentation in this module has at times been copy-pasted directly from the [libzip
-- documentation](https://libzip.org/documentation/).
module Streamly.External.Zip
  ( -- ** Open archive
    Zip,
    OpenFlag (..),
    openZip,

    -- ** Archive information
    getNumEntries,

    -- ** Paths
    PathFlag (..),
    getPathByIndex,

    -- ** Streamly
    GetFileFlag (..),
    unfoldFileAtPath,
    unfoldFileAtIndex,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString
import Data.Void
import Foreign
import Foreign.C.String
import Streamly.External.Zip.Internal
import Streamly.External.Zip.Internal.Foreign
import Streamly.Internal.Data.Unfold.Type

-- | Opens the zip archive at the given file path.
--
-- Thread safety: To satisfy low-level libzip requirements, please use each 'Zip' from one thread
-- only—or manually synchronize its use. Note that it is perfectly fine to open multiple 'Zip's for
-- a single zip file on disk.
openZip :: FilePath -> [OpenFlag] -> IO Zip
openZip fp flags =
  -- This library is currently read-only; always open the archive in read-only mode.
  let flags' = zip_rdonly .|. combineFlags openFlags flags
   in withCString fp $ \fpc -> alloca $ \errp -> mask_ $ do
        zipp <- c_zip_open fpc flags' errp
        if zipp == nullPtr
          then error "todo: throw exception"
          else Zip <$> newForeignPtr c_zip_discard_ptr zipp

-- | Gets the number of entries in the given archive.
getNumEntries :: Zip -> IO Int
getNumEntries (Zip zipfp) =
  let flags' = 0 -- combineFlags numEntriesFlags flags (see (*) in Streamly.External.Zip.Internal).
   in do
        num <- withForeignPtr zipfp $ \zipp -> c_zip_get_num_entries zipp flags'
        if num < 0
          then -- c_zip_get_num_entries should not return -1 here, for zipp is known to be non-NULL.
            error "todo: throw exception"
          else return $ fromIntegral num

-- | Gets the path (e.g., @"foo.txt"@, @"foo/"@, or @"foo/bar.txt"@) of the file at the given
-- 0-based index in the given zip archive. Please use 'getNumEntries' to find the upper bound for
-- the index.
getPathByIndex :: Zip -> Int -> [PathFlag] -> IO ByteString
getPathByIndex (Zip zipfp) idx flags =
  let flags' = combineFlags pathFlags flags
   in do
        name <- withForeignPtr zipfp $ \zipp -> c_zip_get_name zipp (fromIntegral idx) flags'
        if name == nullPtr
          then error "todo: throw exception"
          else packCString name

-- | Creates an @Unfold@ with which we can stream data out of the entry at the given path (e.g.,
-- @"foo.txt"@, @"foo/"@, or @"foo/bar.txt"@).
unfoldFileAtPath :: (MonadIO m) => Zip -> [GetFileFlag] -> String -> Unfold m Void ByteString
unfoldFileAtPath z flags path = unfoldFile z flags (Left path)

-- | Creates an @Unfold@ with which we can stream data out of the entry at the given index. Please
-- use 'getNumEntries' to find the upper bound for the index.
unfoldFileAtIndex :: (MonadIO m) => Zip -> [GetFileFlag] -> Int -> Unfold m Void ByteString
unfoldFileAtIndex z flags idx = unfoldFile z flags (Right idx)
