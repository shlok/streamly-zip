{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

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
import Data.Time.Clock
import Data.Time.Clock.POSIX
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

-- | /Internal/.
getFileFlags :: Map GetFileFlag Zip_flags_t
getFileFlags =
  -- For now, we don’t bother with the name-lookup flags documented at
  -- https://libzip.org/documentation/zip_name_locate.html.
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
    -- Make sure z stays alive for successful filep closure.
    ret <- keepAlive z (c_zip_fclose filep)

    when (ret /= 0) $
      throwError
        "Error closing file"
        (printf "zip_fclose() return code: %d" (fromIntegral @_ @Int ret))
  return $ File filep ref

-- | /Internal/.
{-# INLINE unfoldFile #-}
unfoldFile :: (MonadIO m) => Unfold m (Zip, [GetFileFlag], Either String Int) ByteString
unfoldFile =
  U.mkUnfoldM
    ( \(z, file@(File filep fileFinalizer), bufp, ref) -> liftIO $ do
        bytesRead <- keepAlive z (c_zip_fread filep bufp chunkSize)
        if bytesRead < 0
          then
            throwError
              "Error reading file"
              (printf "zip_fread() return value: %d" (fromIntegral @_ @Int bytesRead))
          else
            if bytesRead == 0
              then do
                runIOFinalizer ref

                -- (Sidenote/TODO: The alternative of commenting this and uncommenting the code at
                -- (**) did not seem to work (runtime crashes; segfaults etc.). For now, we are not
                -- investigating this further.)
                runIOFinalizer fileFinalizer

                return U.Stop
              else do
                bs <- B.packCStringLen (bufp, fromIntegral bytesRead)
                return $ U.Yield bs (z, file, bufp, ref)
    )
    ( \(z, flags, pathOrIndex) -> liftIO $ mask_ $ do
        file@(File _ _) <- getFileByPathOrIndex z flags pathOrIndex
        bufp <- mallocBytes $ fromIntegral chunkSize
        ref <- newIOFinalizer $ do
          free bufp
        --   runIOFinalizer fileFinalizer -- Ref (**).
        return (z, file, bufp, ref)
    )

-- | Information about a file within a zip archive.
newtype FileInfo
  = -- A ForeignPtr works because free() returns void.
    FileInfo (ForeignPtr Zip_stat_t)

-- | /Internal/.
getFileInfoByPathOrIndex :: Zip -> Either String Int -> IO FileInfo
getFileInfoByPathOrIndex (Zip zipfp) pathOrIdx = do
  -- Similarly to getFileByPathOrIndex, we for now don’t bother with the name-lookup flags
  -- documented at https://libzip.org/documentation/zip_name_locate.html. (And since
  -- @ZIP_FL_UNCHANGED@ is the only other applicable flag, but not used by streamly-zip because it’s
  -- read-only (see (*)), there is no flags parameter.)
  fi@(FileInfo zsfp) <- mask_ $ do
    zsp <- malloc @Zip_stat_t
    FileInfo <$> newForeignPtr finalizerFree zsp
  withForeignPtr zipfp $ \zipp -> withForeignPtr zsfp $ \zsp -> do
    case pathOrIdx of
      Left path -> withCString path $ \pathc -> do
        c_zip_stat zipp pathc 0 zsp
          >>= \r -> when (r /= 0) $ do
            err <- BC.unpack <$> (B.packCString =<< c_zip_strerror zipp)
            throwError "Error getting FileInfo at path" err
        return fi
      Right idx -> do
        c_zip_stat_index zipp (fromIntegral idx) 0 zsp
          >>= \r -> when (r /= 0) $ do
            err <- BC.unpack <$> (B.packCString =<< c_zip_strerror zipp)
            throwError "Error getting FileInfo at index" err
        return fi

-- | Gets file name from a @FileInfo@; @Nothing@ if not available.
getFileName :: FileInfo -> IO (Maybe String)
getFileName (FileInfo zsfp) = withForeignPtr zsfp $ \zsp -> do
  zs <- peek zsp
  if s_valid zs .&. zip_stat_name /= 0
    then Just <$> peekCString (s_name zs)
    else return Nothing

-- | Gets index within archive from a @FileInfo@; @Nothing@ if not available.
getFileIndex :: FileInfo -> IO (Maybe Int)
getFileIndex (FileInfo zsfp) = withForeignPtr zsfp $ \zsp -> do
  zs <- peek zsp
  return $
    if s_valid zs .&. zip_stat_index /= 0
      then Just . fromIntegral $ s_index zs
      else Nothing

-- | Gets uncompressed file size from a @FileInfo@; @Nothing@ if not available.
getFileSize :: FileInfo -> IO (Maybe Int)
getFileSize (FileInfo zsfp) = withForeignPtr zsfp $ \zsp -> do
  zs <- peek zsp
  return $
    if s_valid zs .&. zip_stat_size /= 0
      then Just . fromIntegral $ s_size zs
      else Nothing

-- | Gets compressed file size from a @FileInfo@; @Nothing@ if not available.
getFileCompressedSize :: FileInfo -> IO (Maybe Int)
getFileCompressedSize (FileInfo zsfp) = withForeignPtr zsfp $ \zsp -> do
  zs <- peek zsp
  return $
    if s_valid zs .&. zip_stat_comp_size /= 0
      then Just . fromIntegral $ s_comp_size zs
      else Nothing

-- | Gets modification time from a @FileInfo@; @Nothing@ if not available.
getFileModificationTime :: FileInfo -> IO (Maybe UTCTime)
getFileModificationTime (FileInfo zsfp) = withForeignPtr zsfp $ \zsp -> do
  zs <- peek zsp
  return $
    if s_valid zs .&. zip_stat_mtime /= 0
      then let CTime secs = s_mtime zs in Just . posixSecondsToUTCTime . fromIntegral $ secs
      else Nothing

-- | Gets CRC checksum from a @FileInfo@; @Nothing@ if not available.
getFileCRC :: FileInfo -> IO (Maybe Word32)
getFileCRC (FileInfo zsfp) = withForeignPtr zsfp $ \zsp -> do
  zs <- peek zsp
  return $
    if s_valid zs .&. zip_stat_crc /= 0
      then Just . fromIntegral $ s_crc zs
      else Nothing

-- | Gets compression method from a @FileInfo@; @Nothing@ if not available.
getFileCompressionMethod :: FileInfo -> IO (Maybe CompressionMethod)
getFileCompressionMethod (FileInfo zsfp) = withForeignPtr zsfp $ \zsp -> do
  zs <- peek zsp
  return $
    if s_valid zs .&. zip_stat_comp_method /= 0
      then Just . fromMaybe CM_Unrecognized $ M.lookup (s_comp_method zs) compressionMethodsMap
      else Nothing

-- | Gets encryption method from a @FileInfo@; @Nothing@ if not available.
getFileEncryptionMethod :: FileInfo -> IO (Maybe EncryptionMethod)
getFileEncryptionMethod (FileInfo zsfp) = withForeignPtr zsfp $ \zsp -> do
  zs <- peek zsp
  return $
    if s_valid zs .&. zip_stat_encryption_method /= 0
      then Just . fromMaybe EM_Unrecognized $ M.lookup (s_encryption_method zs) encryptionMethodsMap
      else Nothing

-- | Compression method.
data CompressionMethod
  = -- https://github.com/nih-at/libzip/blob/e16526dbb751dc31129b32ba667ce6aed5c42f97/lib/zip.h
    CM_STORE
  | CM_SHRINK
  | CM_REDUCE_1
  | CM_REDUCE_2
  | CM_REDUCE_3
  | CM_REDUCE_4
  | CM_IMPLODE
  | CM_DEFLATE
  | CM_DEFLATE64
  | CM_PKWARE_IMPLODE
  | CM_BZIP2
  | CM_LZMA
  | CM_TERSE
  | CM_LZ77
  | CM_LZMA2
  | CM_ZSTD
  | CM_XZ
  | CM_JPEG
  | CM_WAVPACK
  | CM_PPMD
  | -- | Please do not rely on an unrecognized compression method staying such in future versions of
    -- libzip or streamly-zip.
    CM_Unrecognized
  deriving (Eq, Show)

-- | /Internal/.
compressionMethodsMap :: Map Zip_uint16_t CompressionMethod
compressionMethodsMap =
  M.fromList
    [ (0, CM_STORE),
      (1, CM_SHRINK),
      (2, CM_REDUCE_1),
      (3, CM_REDUCE_2),
      (4, CM_REDUCE_3),
      (5, CM_REDUCE_4),
      (6, CM_IMPLODE),
      (8, CM_DEFLATE),
      (9, CM_DEFLATE64),
      (10, CM_PKWARE_IMPLODE),
      (12, CM_BZIP2),
      (14, CM_LZMA),
      (18, CM_TERSE),
      (19, CM_LZ77),
      (33, CM_LZMA2),
      (93, CM_ZSTD),
      (95, CM_XZ),
      (96, CM_JPEG),
      (97, CM_WAVPACK),
      (98, CM_PPMD)
    ]

-- | Encryption method.
data EncryptionMethod
  = -- https://github.com/nih-at/libzip/blob/e16526dbb751dc31129b32ba667ce6aed5c42f97/lib/zip.h
    EM_NONE
  | EM_TRAD_PKWARE
  | EM_DES
  | EM_RC2_OLD
  | EM_3DES_168
  | EM_3DES_112
  | EM_PKZIP_AES_128
  | EM_PKZIP_AES_192
  | EM_PKZIP_AES_256
  | EM_RC2
  | EM_RC4
  | EM_AES_128
  | EM_AES_192
  | EM_AES_256
  | EM_UNKNOWN
  | -- | Please do not rely on an unrecognized encryption method staying such in future versions of
    -- libzip or streamly-zip.
    EM_Unrecognized
  deriving (Eq, Show)

-- | /Internal/.
encryptionMethodsMap :: Map Zip_uint16_t EncryptionMethod
encryptionMethodsMap =
  M.fromList
    [ (0, EM_NONE),
      (1, EM_TRAD_PKWARE),
      (0x6601, EM_DES),
      (0x6602, EM_RC2_OLD),
      (0x6603, EM_3DES_168),
      (0x6609, EM_3DES_112),
      (0x660e, EM_PKZIP_AES_128),
      (0x660f, EM_PKZIP_AES_192),
      (0x6610, EM_PKZIP_AES_256),
      (0x6702, EM_RC2),
      (0x6801, EM_RC4),
      (0x0101, EM_AES_128),
      (0x0102, EM_AES_192),
      (0x0103, EM_AES_256),
      (0xffff, EM_UNKNOWN)
    ]

-- | /Internal/.
{-# INLINE chunkSize #-}
chunkSize :: Zip_uint64_t
chunkSize = 64000

-- | /Internal/.
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

-- | Gets information about a file at the given path (e.g., @"foo.txt"@, @"foo/"@, or
-- @"foo/bar.txt"@) in the given zip archive.
getFileInfoAtPath :: Zip -> String -> IO FileInfo
getFileInfoAtPath z p = getFileInfoByPathOrIndex z (Left p)

-- | Gets information about a file at the given index in the given zip archive. Please use
-- 'getNumEntries' to find the upper bound for the index.
getFileInfoAtIndex :: Zip -> Int -> IO FileInfo
getFileInfoAtIndex z idx = getFileInfoByPathOrIndex z (Right idx)

-- | /Internal/.
{-# INLINE touch #-}
touch :: a -> IO ()
touch x = IO $ \s -> case touch# x s of
  s' -> (# s', () #)

-- | /Internal/.
{-# INLINE keepAlive #-}
keepAlive :: a -> IO b -> IO b
keepAlive a f = IO $ \s0 -> keepAlive# a s0 (unIO f)
