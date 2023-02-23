{-# LANGUAGE CPP #-}

-- |
-- Module      : Unicode.Internal.Bits
-- Copyright   : (c) 2020 Andrew Lelechenko
--               (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, static bitmap lookup utilities

module Unicode.Internal.Bits.Names
    ( -- * Bitmap lookup
      lookupInt32#
      -- * CString
    , unpackCString#
    ) where

#include "MachDeps.h"

import GHC.Exts (Addr#, Int#)

#ifdef WORDS_BIGENDIAN

import GHC.Exts (narrow32Word#, word2Int#, byteSwap32#, indexWord32OffAddr#)
#if MIN_VERSION_base(4,16,0)
import GHC.Exts (word32ToWord#)
#endif

#else

import GHC.Exts (indexInt32OffAddr#)
#if MIN_VERSION_base(4,16,0)
import GHC.Exts (int32ToInt#)
#endif

#endif

#if MIN_VERSION_base(4,15,0)
import GHC.Exts (unpackCString#)
#else
import GHC.CString (unpackCString#)
#endif

{-| @lookupInt32# addr index@ looks up for the @index@-th 32-bits word in
the bitmap starting at @addr@, then convert it to an 'Int#'.

The caller must make sure that:

* @ceiling (addr + (n * 32))@ is legally accessible 'GHC.Exts.Word32#'.

@since 0.4.1
-}
lookupInt32#
  :: Addr# -- ^ Bitmap address
  -> Int#  -- ^ Word index
  -> Int#  -- ^ Resulting int
lookupInt32#
#ifdef WORDS_BIGENDIAN
#if MIN_VERSION_base(4,16,0)
    addr# k# = word2Int# (narrow32Word# (byteSwap32# (word32ToWord# (indexWord32OffAddr# addr# k#))))
#else
    addr# k# = word2Int# (narrow32Word# (byteSwap32# (indexWord32OffAddr# addr# k#)))
#endif
#elif MIN_VERSION_base(4,16,0)
    addr# k# = int32ToInt# (indexInt32OffAddr# addr# k#)
#else
    = indexInt32OffAddr#
#endif
