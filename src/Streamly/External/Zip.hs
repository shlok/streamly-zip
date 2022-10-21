{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The documentation in this module has at times been copy-pasted directly from the [libzip
-- documentation](https://libzip.org/documentation/).
module Streamly.External.Zip
  ( -- ** Open archive
    Zip,
    OpenFlag (..),
    withZip,

    -- ** Archive information
    getNumEntries,

    -- ** File within an archive
    PathFlag (..),
    getPathByIndex,
    File,
    GetFileFlag (..),
    withFileByPath,
    withFileByIndex,

    -- ** Streamly
    unfoldFile,
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.ByteString (ByteString, packCString, packCStringLen)
import Data.Void (Void)
import Foreign (nullPtr)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Streamly.External.Zip.Internal
import Streamly.External.Zip.Internal.Foreign
import Streamly.Internal.Data.Stream.StreamD.Type (Step (..))
import Streamly.Internal.Data.Unfold (supply)
import Streamly.Internal.Data.Unfold.Type (Unfold (..))

-- | Operate on the zip archive at the given file path.
withZip :: FilePath -> [OpenFlag] -> (Zip -> IO a) -> IO a
withZip fp flags io =
  -- This library is currently read-only; always open the archive in read-only mode.
  let flags' = zip_rdonly .|. combineFlags openFlags flags
   in withCString fp $ \fpc -> alloca $ \errp ->
        bracket
          ( do
              zipp <- c_zip_open fpc flags' errp
              if zipp == nullPtr
                then error "todo: throw exception"
                else return zipp
          )
          c_zip_discard
          (io . Zip)

-- | Gets the number of entries in the given archive.
getNumEntries :: Zip -> IO Int
getNumEntries (Zip zipp) =
  let flags' = 0 -- combineFlags numEntriesFlags flags
   in do
        num <- c_zip_get_num_entries zipp flags'
        if num < 0
          then -- c_zip_get_num_entries should not return -1 here, for zipp is known to be non-NULL.
            error "todo: throw exception"
          else return $ fromIntegral num

-- | Gets the path (e.g., @"foo.txt"@, @"foo/"@, or @"foo/bar.txt"@) of the file at the given
-- 0-based index in the given zip archive. Use 'getNumEntries' to find the upper bound for the
-- index.
getPathByIndex :: Zip -> Int -> [PathFlag] -> IO ByteString
getPathByIndex (Zip zipp) idx flags =
  let flags' = combineFlags pathFlags flags
   in do
        name <- c_zip_get_name zipp (fromIntegral idx) flags'
        if name == nullPtr
          then error "todo: throw exception"
          else packCString name

-- | Operate on the 'File' at the given path within the given archive.
withFileByPath :: Zip -> [GetFileFlag] -> String -> (File -> IO a) -> IO a
withFileByPath zip' flags path = withFileByPathOrIndex zip' flags (Left path)

-- | Operate on the 'File' at the given index within the given archive.
withFileByIndex :: Zip -> [GetFileFlag] -> Int -> (File -> IO a) -> IO a
withFileByIndex zip' flags idx = withFileByPathOrIndex zip' flags (Right idx)

-- | An @Unfold@ with which we can stream data out of the given file.
{-# INLINE unfoldFile #-}
unfoldFile :: (MonadIO m) => File -> Unfold m Void ByteString
unfoldFile (File filep) =
  supply () $
    Unfold
      ( \buffp -> liftIO . withForeignPtr buffp $ \bufp -> do
          bytesRead <- c_zip_fread filep bufp chunkSize
          if bytesRead < 0
            then error "todo: throw exception"
            else
              if bytesRead == 0
                then return Stop
                else do
                  bs <- packCStringLen (bufp, fromIntegral bytesRead)
                  return $ Yield bs buffp
      )
      (\() -> liftIO $ mallocForeignPtrBytes @CChar (fromIntegral chunkSize))
