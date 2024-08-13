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

    -- ** Streamly
    GetFileFlag (..),
    unfoldFileAtPath,
    unfoldFileAtIndex,
  )
where

import Streamly.External.Zip.Internal
