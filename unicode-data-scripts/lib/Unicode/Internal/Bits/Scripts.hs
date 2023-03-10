{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

-- |
-- Module      : Unicode.Internal.Bits.Scripts
-- Copyright   : (c) 2023 Pierre Le Marre
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, static bitmap lookup utilities

module Unicode.Internal.Bits.Scripts
    ( -- * Bitmap lookup
      lookupIntN#
    , nextInt8#
    , nextInt32#
      -- * CString
    , unpackCString#
    ) where

#include "MachDeps.h"

import GHC.Exts (Addr#, Int#, Int(..), indexWord8OffAddr#, word2Int#)

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

#if MIN_VERSION_base(4,16,0)
import GHC.Exts (word8ToWord#)
#endif

{-| @lookupIntN# addr index@ looks up for the @index@-th @8@-bits word in
the bitmap starting at @addr@, then convert it to an 'Int'.

The caller must make sure that:

* @ceiling (addr + (n * 8))@ is legally accessible 'GHC.Exts.Word8#'.

@since 0.3.0
-}
lookupIntN#
  :: Addr# -- ^ Bitmap address
  -> Int   -- ^ Word index
  -> Int#  -- ^ Resulting word as 'Int'
lookupIntN# addr# (I# index#) = word2Int#
#if MIN_VERSION_base(4,16,0)
    (word8ToWord# (indexWord8OffAddr# addr# index#))
#else
    (indexWord8OffAddr# addr# index#)
#endif

{-| @nextInt8# addr@ looks up for the 8-bits word in
the bitmap starting at @addr@, then convert it to an 'Int#'.

@since 0.3.0
-}
nextInt8# :: Addr# -> Int#
nextInt8# addr# = word2Int#
#if MIN_VERSION_base(4,16,0)
    (word8ToWord# (indexWord8OffAddr# addr# 0#))
#else
    (indexWord8OffAddr# addr# 0#)
#endif

{-| @nextInt32# addr@ looks up for the 32-bits word in
the bitmap starting at @addr@, then convert it to an 'Int#'.

@since 0.3.0
-}
nextInt32#
  :: Addr# -- ^ Bitmap address
  -> Int#  -- ^ Resulting int
nextInt32# addr# =
#ifdef WORDS_BIGENDIAN
#if MIN_VERSION_base(4,16,0)
    word2Int# (narrow32Word# (byteSwap32# (word32ToWord# (indexWord32OffAddr# addr# 0#))))
#else
    word2Int# (narrow32Word# (byteSwap32# (indexWord32OffAddr# addr# 0#)))
#endif
#elif MIN_VERSION_base(4,16,0)
    int32ToInt# (indexInt32OffAddr# addr# 0#)
#else
    indexInt32OffAddr# addr# 0#
#endif
