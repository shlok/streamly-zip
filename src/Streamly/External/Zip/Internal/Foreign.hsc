module Streamly.External.Zip.Internal.Foreign where

import Data.Int
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <zip.h>
type Zip_flags_t = #type zip_flags_t
type Zip_int64_t = #type zip_int64_t
type Zip_uint64_t = #type zip_uint64_t

zip_error_t_size :: Int
zip_error_t_size = #size zip_error_t

data Zip_t

data Zip_file_t

data Zip_error_t

foreign import ccall safe "zip.h zip_open"
  c_zip_open :: CString -> CInt -> Ptr CInt -> IO (Ptr Zip_t)

foreign import ccall safe "zip.h &zip_discard"
  c_zip_discard_ptr :: FunPtr (Ptr Zip_t -> IO ())

-- As this library currently only does reading, we don’t export zip_close().

foreign import ccall safe "zip.h zip_get_num_entries"
  c_zip_get_num_entries :: Ptr Zip_t -> Zip_flags_t -> IO Zip_int64_t

foreign import ccall safe "zip.h zip_get_name"
  c_zip_get_name :: Ptr Zip_t -> Zip_uint64_t -> Zip_flags_t -> IO (Ptr CChar)

foreign import ccall safe "zip.h zip_fopen"
  c_zip_fopen :: Ptr Zip_t -> CString -> Zip_flags_t -> IO (Ptr Zip_file_t)

foreign import ccall safe "zip.h zip_fopen_index"
  c_zip_fopen_index :: Ptr Zip_t -> Zip_uint64_t -> Zip_flags_t -> IO (Ptr Zip_file_t)

foreign import ccall safe "zip.h zip_fclose"
  c_zip_fclose :: Ptr Zip_file_t -> IO CInt

foreign import ccall safe "zip.h zip_fread"
  c_zip_fread :: Ptr Zip_file_t -> Ptr CChar -> Zip_uint64_t -> IO Zip_int64_t

foreign import ccall safe "zip.h zip_strerror"
  c_zip_strerror :: Ptr Zip_t -> IO (Ptr CChar)

-- foreign import ccall safe "zip.h zip_file_strerror"
--   c_zip_file_strerror :: Ptr Zip_file_t -> IO (Ptr CChar)

foreign import ccall safe "zip.h zip_error_init_with_code"
  c_zip_error_init_with_code :: Ptr Zip_error_t -> CInt -> IO ()

foreign import ccall safe "zip.h zip_error_strerror"
  c_zip_error_strerror :: Ptr Zip_error_t -> IO (Ptr CChar)

foreign import ccall safe "zip.h zip_error_fini"
  c_zip_error_fini :: Ptr Zip_error_t -> IO ()

-- All flags relevant for the libzip functions we use.
zip_checkcons,
  zip_create,
  zip_excl,
  zip_truncate,
  zip_rdonly,
  zip_fl_compressed,
  zip_fl_unchanged,
  zip_fl_enc_raw,
  zip_fl_enc_guess,
  zip_fl_enc_strict ::
    (Num a) => a
zip_checkcons = #const ZIP_CHECKCONS
zip_create = #const ZIP_CREATE
zip_excl = #const ZIP_EXCL
zip_truncate = #const ZIP_TRUNCATE
zip_rdonly = #const ZIP_RDONLY
zip_fl_compressed = #const ZIP_FL_COMPRESSED
zip_fl_unchanged = #const ZIP_FL_UNCHANGED
zip_fl_enc_raw = #const ZIP_FL_ENC_RAW
zip_fl_enc_guess = #const ZIP_FL_ENC_GUESS
zip_fl_enc_strict = #const ZIP_FL_ENC_STRICT
