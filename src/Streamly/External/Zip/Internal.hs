{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}

module Streamly.External.Zip.Internal where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Exts
import GHC.IO hiding (liftIO)
import Streamly.Data.Unfold (Unfold)
import Streamly.External.Zip.Internal.Error
import Streamly.External.Zip.Internal.Foreign
import Streamly.Internal.Data.IOFinalizer
import qualified Streamly.Internal.Data.Unfold as U
import Text.Printf

-- | A zip archive.
newtype Zip
  = -- A ForeignPtr works because zip_discard() returns void.
    Zip (ForeignPtr Zip_t)

-- (*) Certain libzip functionality (e.g., flags) has been commented out because it is currently not
-- applicable for this library, e.g., because this library is currently read-only.

data OpenFlag
  = -- Most libzip flags not included; see (*).

    -- | Perform additional stricter consistency checks on the archive, and error if they fail.
    O_CHECKCONS
  deriving (Eq, Ord)

-- | /Internal/.
openFlags :: Map OpenFlag CInt
openFlags =
  M.fromList
    [ (O_CHECKCONS, zip_checkcons)
    -- (O_CREATE, zip_create), -- See (*).
    -- (O_EXCL, zip_excl),
    -- (O_TRUNCATE, zip_truncate)
    -- (O_RDONLY, zip_rdonly)
    ]

-- | Opens the zip archive at the given file path.
--
-- /Warning/: To satisfy low-level libzip requirements, please use each 'Zip' from one thread
-- only—or make sure to synchronize its use. Note that it is perfectly fine to open multiple 'Zip's
-- for a single zip file on disk.
openZip :: FilePath -> [OpenFlag] -> IO Zip
openZip fp flags =
  -- This library is currently read-only; always open the archive in read-only mode; see (*).
  let flags' = zip_rdonly .|. combineFlags openFlags flags
   in withCString fp $ \fpc -> alloca $ \errp -> mask_ $ do
        zipp <- c_zip_open fpc flags' errp
        if zipp == nullPtr
          then do
            err <- libzipErrToString =<< peek errp
            throwError "openZip" err
          else Zip <$> newForeignPtr c_zip_discard_ptr zipp

-- See (*).
-- data NumEntriesFlag
--   = -- | The original number of entries is returned.
--     NE_FL_UNCHANGED
--   deriving (Eq, Ord)

-- numEntriesFlags :: Map NumEntriesFlag Zip_flags_t
-- numEntriesFlags =
--   M.fromList
--     [(NEF_FL_UNCHANGED, zip_fl_unchanged)]

-- getNumEntries :: Zip -> [NumEntriesFlag] -> IO Int

-- | Gets the number of entries in the given archive.
getNumEntries :: Zip -> IO Int
getNumEntries (Zip zipfp) =
  let flags' = 0 -- combineFlags numEntriesFlags flags; see (*).
   in do
        num <- withForeignPtr zipfp $ \zipp -> c_zip_get_num_entries zipp flags'
        if num < 0
          then -- c_zip_get_num_entries should not return -1 here, for zipp is known to be non-NULL.
            throwError "getNumEntries" "unexpected"
          else return $ fromIntegral num

data PathFlag
  = --
    --   -- | The original unchanged filename is returned. -- See (*).
    --   P_FL_UNCHANGED

    -- | Return the unmodified names as it is in the ZIP archive.
    P_FL_ENC_RAW
  | -- | (Default.) Guess the encoding of the name in the ZIP archive and convert it to UTF-8, if
    -- necessary. (Only CP-437 and UTF-8 are recognized.)
    P_FL_ENC_GUESS
  | -- | Follow the ZIP specification and expect CP-437 encoded names in the ZIP archive (except if
    -- they are explicitly marked as UTF-8). Convert it to UTF-8.
    P_FL_ENC_STRICT
  deriving (Eq, Ord)

-- | /Internal/.
pathFlags :: Map PathFlag Zip_flags_t
pathFlags =
  M.fromList
    [ -- (P_FL_UNCHANGED, zip_fl_unchanged) -- See (*).
      (P_FL_ENC_RAW, zip_fl_enc_raw),
      (P_FL_ENC_GUESS, zip_fl_enc_guess),
      (P_FL_ENC_STRICT, zip_fl_enc_strict)
    ]

-- | Gets the path (e.g., @"foo.txt"@, @"foo/"@, or @"foo/bar.txt"@) of the file at the given
-- 0-based index in the given zip archive. Please use 'getNumEntries' to find the upper bound for
-- the index.
getPathByIndex :: Zip -> Int -> [PathFlag] -> IO ByteString
getPathByIndex (Zip zipfp) idx flags =
  let flags' = combineFlags pathFlags flags
   in withForeignPtr zipfp $ \zipp -> do
        name <- c_zip_get_name zipp (fromIntegral idx) flags'
        if name == nullPtr
          then do
            err <- BC.unpack <$> (B.packCString =<< c_zip_strerror zipp)
            throwError "getPathByIndex" err
          else B.packCString name

-- | A file inside of a 'Zip' archive.
--
-- /Internal/.
data File
  = File
      !(Ptr Zip_file_t) -- ForeignPtr does not work because zip_fclose() does not return void.
      !IOFinalizer

data GetFileFlag
  = -- | Read the compressed data. Otherwise the data is uncompressed when reading.
    GF_FL_COMPRESSED
  -- -- | Read the original data from the zip archive, ignoring any changes made to the file.
  --  GF_FL_UNCHANGED
  deriving (Eq, Ord)

-- /Internal/.
getFileFlags :: Map GetFileFlag Zip_flags_t
getFileFlags =
  M.fromList
    [ (GF_FL_COMPRESSED, zip_fl_compressed)
    -- (GF_FL_UNCHANGED, zip_fl_unchanged)
    ]

-- | We don't publicly expose getting a 'File' (and then unfolding from it) because we don't want
-- users to unfold from the same 'File' more than once. (libzip’s @c_zip_fread@ isn’t designed for
-- iterating through a file more than once.)
--
-- /Internal/.
getFileByPathOrIndex :: Zip -> [GetFileFlag] -> Either String Int -> IO File
getFileByPathOrIndex z@(Zip zipfp) flags pathOrIdx = mask_ $ do
  let flags' = combineFlags getFileFlags flags
  filep <- case pathOrIdx of
    Left path ->
      withCString path $ \pathc -> withForeignPtr zipfp $ \zipp -> do
        filep <- c_zip_fopen zipp pathc flags'
        when (filep == nullPtr) $ do
          err <- BC.unpack <$> (B.packCString =<< c_zip_strerror zipp)
          throwError "Error opening file at path" err
        return filep
    Right idx ->
      withForeignPtr zipfp $ \zipp -> do
        filep <- c_zip_fopen_index zipp (fromIntegral idx) flags'
        when (filep == nullPtr) $ do
          err <- BC.unpack <$> (B.packCString =<< c_zip_strerror zipp)
          throwError "Error opening file at index" err
        return filep
  ref <- newIOFinalizer $ do
    ret <- IO $ \s0 ->
      -- Make sure z stays alive for successful filep closure.
      keepAlive# z s0 (unIO $ c_zip_fclose filep)
    when (ret /= 0) $
      throwError
        "Error closing file"
        (printf "zip_fclose() return code: %d" (fromIntegral @_ @Int ret))
  return $ File filep ref

-- /Internal/.
{-# INLINE unfoldFile #-}
unfoldFile :: (MonadIO m) => Unfold m (Zip, [GetFileFlag], Either String Int) ByteString
unfoldFile =
  U.mkUnfoldM
    ( \(z, file@(File filep _), bufp, ref) -> liftIO $ do
        bytesRead <- c_zip_fread filep bufp chunkSize
        if bytesRead < 0
          then
            throwError
              "Error reading file"
              (printf "zip_fread() return value: %d" (fromIntegral @_ @Int bytesRead))
          else
            if bytesRead == 0
              then do
                runIOFinalizer ref
                return U.Stop
              else do
                bs <- B.packCStringLen (bufp, fromIntegral bytesRead)
                return $ U.Yield bs (z, file, bufp, ref)
    )
    ( \(z, flags, pathOrIndex) -> liftIO $ mask_ $ do
        file@(File _ fileFinalizer) <- getFileByPathOrIndex z flags pathOrIndex
        bufp <- mallocBytes $ fromIntegral chunkSize
        ref <- newIOFinalizer $ do
          free bufp
          runIOFinalizer fileFinalizer
        return (z, file, bufp, ref)
    )

-- /Internal/.
{-# INLINE chunkSize #-}
chunkSize :: Zip_uint64_t
chunkSize = 64000

-- /Internal/.
combineFlags :: (Ord flagType, Bits a, Num a) => Map flagType a -> [flagType] -> a
combineFlags allFlags =
  foldl'
    (\acc chosenFlag -> acc .|. fromMaybe (error "flag expected") (M.lookup chosenFlag allFlags))
    0

-- | Creates an @Unfold@ with which one can stream data out of the entry at the given path (e.g.,
-- @"foo.txt"@, @"foo/"@, or @"foo/bar.txt"@).
unfoldFileAtPath :: (MonadIO m) => Unfold m (Zip, [GetFileFlag], String) ByteString
unfoldFileAtPath = U.lmap (\(z, fl, p) -> (z, fl, Left p)) unfoldFile

-- | Creates an @Unfold@ with which one can stream data out of the entry at the given index. Please
-- use 'getNumEntries' to find the upper bound for the index.
unfoldFileAtIndex :: (MonadIO m) => Unfold m (Zip, [GetFileFlag], Int) ByteString
unfoldFileAtIndex = U.lmap (\(z, fl, idx) -> (z, fl, Right idx)) unfoldFile
