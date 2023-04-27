-- We want to make things that are not publicly exposed available nonetheless to advanced users;
-- hence this module.
module Streamly.External.Zip.Internal where

import Control.Exception
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.ByteString (ByteString, packCStringLen)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Void
import Foreign
import Foreign.C.String
import Foreign.C.Types (CInt)
import qualified Streamly.Data.Unfold as U
import Streamly.External.Zip.Internal.Foreign
import Streamly.Internal.Data.IOFinalizer
import Streamly.Internal.Data.Stream.StreamD.Type (Step (..))
import Streamly.Internal.Data.Unfold.Type

-- | A zip archive.
newtype Zip
  = -- A ForeignPtr works because zip_discard() returns void.
    Zip (ForeignPtr Zip_t)

-- (*) Certain libzip functionality (e.g., flags) has been commented out because it is currently not
-- applicable for this library, e.g., because this library is currently read-only.

data OpenFlag
  = -- | Perform additional stricter consistency checks on the archive, and error if they fail.
    O_CHECKCONS
  | -- | Create the archive if it does not exist.
    O_CREATE
  | -- | Error if archive already exists.
    O_EXCL
  | -- | If archive exists, ignore its current contents. In other words, handle it the same way as
    -- an empty archive.
    O_TRUNCATE
  -- -- | Open archive in read-only mode.
  --   O_RDONLY
  deriving (Eq, Ord)

-- Not publicly exported.
openFlags :: Map OpenFlag CInt
openFlags =
  M.fromList
    [ (O_CHECKCONS, zip_checkcons),
      (O_CREATE, zip_create),
      (O_EXCL, zip_excl),
      (O_TRUNCATE, zip_truncate)
      -- (O_RDONLY, #const ZIP_RDONLY)
    ]

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

data PathFlag
  = --
    --   -- | The original unchanged filename is returned
    --   P_FL_UNCHANGED

    -- | Return the unmodified names as it is in the ZIP archive.
    P_FL_ENC_RAW
  | -- | (Default.) Guess the encoding of the name in the ZIP archive and convert it to UTF-8, if
    -- necessary. (Only CP-437 and UTF-8 are recognized.)
    P_FL_ENC_GUESS
  | -- | Follow the ZIP specification and expect CP-437 encoded names in the ZIP archive (except
    -- if they are explicitly marked as UTF-8). Convert it to UTF-8.
    P_FL_ENC_STRICT
  deriving (Eq, Ord)

pathFlags :: Map PathFlag Zip_flags_t
pathFlags =
  M.fromList
    [ -- (P_FL_UNCHANGED, zip_fl_unchanged)
      (P_FL_ENC_RAW, zip_fl_enc_raw),
      (P_FL_ENC_GUESS, zip_fl_enc_guess),
      (P_FL_ENC_STRICT, zip_fl_enc_strict)
    ]

-- | A file inside of a 'Zip' archive.
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

getFileFlags :: Map GetFileFlag Zip_flags_t
getFileFlags =
  M.fromList
    [ (GF_FL_COMPRESSED, zip_fl_compressed)
    -- (GF_FL_UNCHANGED, zip_fl_unchanged)
    ]

-- We don't publicly expose getting a 'File' (and then unfolding from it) because we don't want
-- users to unfold from the same 'File' more than once.
getFileByPathOrIndex :: Zip -> [GetFileFlag] -> Either String Int -> IO File
getFileByPathOrIndex (Zip zipfp) flags pathOrIdx = do
  let flags' = combineFlags getFileFlags flags
  filep <- case pathOrIdx of
    Left path ->
      withCString path $ \pathc -> withForeignPtr zipfp $ \zipp ->
        c_zip_fopen zipp pathc flags'
    Right idx ->
      withForeignPtr zipfp $ \zipp ->
        c_zip_fopen_index zipp (fromIntegral idx) flags'
  if filep == nullPtr
    then error $ "todo: throw exception; file could not be opened: " ++ show pathOrIdx
    else do
      ref <- newIOFinalizer $ do
        ret <- c_zip_fclose filep
        when (ret /= 0) $ error "todo: throw exception"
      return $ File filep ref

{-# INLINE unfoldFile #-}
unfoldFile :: (MonadIO m) => Zip -> [GetFileFlag] -> Either String Int -> Unfold m Void ByteString
unfoldFile z@(Zip zipfp) flags pathOrIndex =
  (U.lmap . const) () $
    Unfold
      ( \(file@(File filep _), bufp, ref) -> liftIO $ do
          bytesRead <- c_zip_fread filep bufp chunkSize
          if bytesRead < 0
            then error $ "todo: throw exception " ++ show bytesRead
            else
              if bytesRead == 0
                then do
                  runIOFinalizer ref
                  touchForeignPtr zipfp -- Keep zip alive for (at least) the duration of the Unfold.
                  return Stop
                else do
                  bs <- packCStringLen (bufp, fromIntegral bytesRead)
                  return $ Yield bs (file, bufp, ref)
      )
      ( \() -> liftIO $ mask_ $ do
          file@(File _ fileFinalizer) <- getFileByPathOrIndex z flags pathOrIndex
          bufp <- mallocBytes $ fromIntegral chunkSize
          ref <- newIOFinalizer $ do
            free bufp
            runIOFinalizer fileFinalizer
          return (file, bufp, ref)
      )

{-# INLINE chunkSize #-}
chunkSize :: Zip_uint64_t
chunkSize = 64000

combineFlags :: (Ord flagType, Bits a, Num a) => Map flagType a -> [flagType] -> a
combineFlags allFlags =
  foldl'
    (\acc chosenFlag -> acc .|. fromMaybe (error "flag expected") (M.lookup chosenFlag allFlags))
    0
