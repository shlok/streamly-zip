-- The documentation in this module has at times been copy-pasted directly from the [libzip
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

    -- ** Reading
    GetFileFlag (..),
    unfoldFileAtPath,
    unfoldFileAtIndex,

    -- ** File information
    FileInfo,
    getFileInfoAtPath,
    getFileInfoAtIndex,
    getFileName,
    getFileIndex,
    getFileSize,
    getFileCompressedSize,
    getFileModificationTime,
    getFileCRC,
    getFileCompressionMethod,
    getFileEncryptionMethod,
    CompressionMethod (..),
    EncryptionMethod (..),
  )
where

import Streamly.External.Zip.Internal
