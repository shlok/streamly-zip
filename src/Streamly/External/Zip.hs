{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The documentation in this module has at times been copy-pasted directly from the [libzip
-- documentation](https://libzip.org/documentation/).
module Streamly.External.Zip
  ( -- ** Open archive
    Zip,
    OpenFlag (..),
    openZip,

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

import Control.Exception (mask_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.ByteString (ByteString, packCString, packCStringLen)
import Data.Void (Void)
import Foreign (nullPtr)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar)
import Foreign.ForeignPtr (mallocForeignPtrBytes, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Streamly.External.Zip.Internal
import Streamly.External.Zip.Internal.Foreign
import Streamly.Internal.Data.Stream.StreamD.Type (Step (..))
import Streamly.Internal.Data.Unfold (supply)
import Streamly.Internal.Data.Unfold.Type (Unfold (..))

-- | Opens the zip archive at the given file path.
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
  let flags' = 0 -- combineFlags numEntriesFlags flags
   in withForeignPtr zipfp $ \zipp ->
        -- c_zip_get_num_entries will not return -1 here because zipp is known to be non-NULL.
        fromIntegral <$> c_zip_get_num_entries zipp flags'

-- | Gets the path (e.g., @"foo.txt"@, @"foo/"@, or @"foo/bar.txt"@) of the file at the given
-- 0-based index in the given zip archive. Use 'getNumEntries' to find the upper bound for the
-- index.
getPathByIndex :: Zip -> Int -> [PathFlag] -> IO ByteString
getPathByIndex (Zip zipfp) idx flags =
  let flags' = combineFlags pathFlags flags
   in withForeignPtr zipfp $ \zipp ->
        packCString =<< c_zip_get_name zipp (fromIntegral idx) flags'

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
