{-# LANGUAGE NamedFieldPuns #-}
module Streamly.External.Zip.Internal.Foreign where

import Data.Int
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <zip.h>
type Zip_flags_t = #type zip_flags_t
type Zip_int64_t = #type zip_int64_t
type Zip_uint16_t = #type zip_uint16_t
type Zip_uint32_t = #type zip_uint32_t
type Zip_uint64_t = #type zip_uint64_t

zip_error_t_size :: Int
zip_error_t_size = #size zip_error_t

data Zip_t

data Zip_file_t

data Zip_error_t

data Zip_stat_t = Zip_stat_t
  { s_valid :: {-# UNPACK #-} !Zip_uint64_t,
    s_name :: {-# UNPACK #-} !CString,
    s_index :: {-# UNPACK #-} !Zip_uint64_t,
    s_size :: {-# UNPACK #-} !Zip_uint64_t,
    s_comp_size :: {-# UNPACK #-} !Zip_uint64_t,
    s_mtime :: {-# UNPACK #-} !CTime,
    s_crc :: {-# UNPACK #-} !Zip_uint32_t,
    s_comp_method :: {-# UNPACK #-} !Zip_uint16_t,
    s_encryption_method :: {-# UNPACK #-} !Zip_uint16_t,
    s_flags :: {-# UNPACK #-} !Zip_uint32_t
  }

instance Storable Zip_stat_t where
  {-# INLINE alignment #-}
  alignment _ = #{alignment zip_stat_t}
  {-# INLINE sizeOf #-}
  sizeOf _ = #{size zip_stat_t}
  {-# INLINE peek #-}
  peek ptr = do
    s_valid <- #{peek zip_stat_t, valid} ptr
    s_name <- #{peek zip_stat_t, name} ptr
    s_index <- #{peek zip_stat_t, index} ptr
    s_size <- #{peek zip_stat_t, size} ptr
    s_comp_size <- #{peek zip_stat_t, comp_size} ptr
    s_mtime <- #{peek zip_stat_t, mtime} ptr
    s_crc <- #{peek zip_stat_t, crc} ptr
    s_comp_method <- #{peek zip_stat_t, comp_method} ptr
    s_encryption_method <- #{peek zip_stat_t, encryption_method} ptr
    s_flags <- #{peek zip_stat_t, flags} ptr
    return $! Zip_stat_t
      { s_valid,
        s_name,
        s_index,
        s_size,
        s_comp_size,
        s_mtime,
        s_crc,
        s_comp_method,
        s_encryption_method,
        s_flags
      }
  {-# INLINE poke #-}
  poke ptr s = do
    #{poke zip_stat_t, valid} ptr (s_valid s)
    #{poke zip_stat_t, name} ptr (s_name s)
    #{poke zip_stat_t, index} ptr (s_index s)
    #{poke zip_stat_t, size} ptr (s_size s)
    #{poke zip_stat_t, comp_size} ptr (s_comp_size s)
    #{poke zip_stat_t, mtime} ptr (s_mtime s)
    #{poke zip_stat_t, crc} ptr (s_crc s)
    #{poke zip_stat_t, comp_method} ptr (s_comp_method s)
    #{poke zip_stat_t, encryption_method} ptr (s_encryption_method s)
    #{poke zip_stat_t, flags} ptr (s_flags s)

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

foreign import ccall safe "zip.h zip_stat_init"
  c_zip_stat_init :: Ptr Zip_stat_t -> IO ()

foreign import ccall safe "zip.h zip_stat"
  c_zip_stat :: Ptr Zip_t -> CString -> Zip_flags_t -> Ptr Zip_stat_t -> IO CInt

foreign import ccall safe "zip.h zip_stat_index"
  c_zip_stat_index :: Ptr Zip_t -> Zip_uint64_t -> Zip_flags_t -> Ptr Zip_stat_t -> IO CInt

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
  zip_fl_enc_strict,
  zip_stat_name,
  zip_stat_index,
  zip_stat_size,
  zip_stat_comp_size,
  zip_stat_mtime,
  zip_stat_crc,
  zip_stat_comp_method,
  zip_stat_encryption_method,
  zip_stat_flags ::
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
zip_stat_name = #const ZIP_STAT_NAME
zip_stat_index = #const ZIP_STAT_INDEX
zip_stat_size = #const ZIP_STAT_SIZE
zip_stat_comp_size = #const ZIP_STAT_COMP_SIZE
zip_stat_mtime = #const ZIP_STAT_MTIME
zip_stat_crc = #const ZIP_STAT_CRC
zip_stat_comp_method = #const ZIP_STAT_COMP_METHOD
zip_stat_encryption_method = #const ZIP_STAT_ENCRYPTION_METHOD
zip_stat_flags = #const ZIP_STAT_FLAGS
