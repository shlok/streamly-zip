-- We want to make things that are not publicly exported accessible to the outside world for
-- advanced users; hence this module.
module Streamly.External.Zip.Internal where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Bits (Bits, (.|.))
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Foreign (Ptr)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Streamly.External.Zip.Internal.Foreign

-- | A zip archive.
newtype Zip = Zip (ForeignPtr Zip_t) -- Constructor not publicly exported.

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
newtype File = File (Ptr Zip_file_t)

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

-- The somewhat unfortunate “withFile” API is due to the fact that c_zip_fclose returns a CInt
-- that has to be handled. (If it returned void, we could have put Zip_file_t into a ForeignPtr.)
withFileByPathOrIndex :: Zip -> [GetFileFlag] -> Either String Int -> (File -> IO a) -> IO a
withFileByPathOrIndex (Zip zipfp) flags pathOrIdx io =
  let flags' = combineFlags getFileFlags flags
   in withForeignPtr zipfp $ \zipp ->
        bracket
          ( case pathOrIdx of
              Left path -> withCString path $ \pathc -> c_zip_fopen zipp pathc flags'
              Right idx -> c_zip_fopen_index zipp (fromIntegral idx) flags'
          )
          ( \filep -> do
              ret <- c_zip_fclose filep
              when (ret /= 0) (error "todo: throw exception")
          )
          (io . File)

{-# INLINE chunkSize #-}
chunkSize :: Zip_uint64_t
chunkSize = 64000

combineFlags :: (Ord flagType, Bits a, Num a) => Map flagType a -> [flagType] -> a
combineFlags allFlags =
  foldl'
    (\acc chosenFlag -> acc .|. fromMaybe (error "flag expected") (M.lookup chosenFlag allFlags))
    0
