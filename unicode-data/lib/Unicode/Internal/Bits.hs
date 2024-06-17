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

module Unicode.Internal.Bits
    ( -- * Bitmap lookup
      lookupBit64,
      lookupWord8AsInt,
      lookupWord16AsInt,
      lookupWord32#,
      -- * CString
      unpackCString#
    ) where

#include "MachDeps.h"

import Data.Bits (finiteBitSize, popCount)
import GHC.Exts
       (Addr#, Int(..), Int#, Word(..), Word#,
        indexWordOffAddr#, indexWord8OffAddr#,
        indexWord16OffAddr#, indexWord32OffAddr#,
        andI#, uncheckedIShiftRL#,
        and#, word2Int#, uncheckedShiftL#)
#if MIN_VERSION_base(4,16,0)
import GHC.Exts (word8ToWord#, word16ToWord#, word32ToWord#)
#endif
#ifdef WORDS_BIGENDIAN
import GHC.Exts
       (byteSwap#, narrow16Word#, narrow32Word#,
        byteSwap16#, byteSwap32#)
#endif

#if MIN_VERSION_base(4,15,0)
import GHC.Exts (unpackCString#)
#else
import GHC.CString (unpackCString#)
#endif

-- | @lookup64 addr index@ looks up the bit stored at bit index @index@ using a
-- bitmap starting at the address @addr@. Looks up the 64-bit word containing
-- the bit and then the bit in that word. The caller must make sure that the
-- 64-bit word at the byte address (addr + index / 64) * 8 is legally
-- accessible memory.
--
lookupBit64 :: Addr# -> Int -> Bool
lookupBit64 addr# (I# index#) = W# (word## `and#` bitMask##) /= 0
  where
    !fbs@(I# fbs#) = finiteBitSize (0 :: Word) - 1
    !(I# logFbs#) = case fbs of
      31 -> 5
      63 -> 6
      _  -> popCount fbs -- this is a really weird architecture

    wordIndex# = index# `uncheckedIShiftRL#` logFbs#
#ifdef WORDS_BIGENDIAN
    word## = byteSwap# (indexWordOffAddr# addr# wordIndex#)
#else
    word## = indexWordOffAddr# addr# wordIndex#
#endif
    bitIndex# = index# `andI#` fbs#
    bitMask## = 1## `uncheckedShiftL#` bitIndex#

{-| @lookupWord8AsInt addr index@ looks up for the @index@-th @8@-bits word in
the bitmap starting at @addr@, then convert it to an 'Int'.

The caller must make sure that:

* @ceiling (addr + (n * 8))@ is legally accessible 'GHC.Exts.Word8#'.

@since 0.3.0
-}
lookupWord8AsInt
  :: Addr# -- ^ Bitmap address
  -> Int   -- ^ Word index
  -> Int   -- ^ Resulting word as 'Int'
lookupWord8AsInt addr# (I# index#) = I# (word2Int# word##)
  where
#if MIN_VERSION_base(4,16,0)
    word## = word8ToWord# (indexWord8OffAddr# addr# index#)
#else
    word## = indexWord8OffAddr# addr# index#
#endif

lookupWord16AsInt
  :: Addr# -- ^ Bitmap address
  -> Int   -- ^ Word index
  -> Int   -- ^ Resulting word as `Int`
lookupWord16AsInt addr# (I# k#) = I# (word2Int# word##)
    where
#ifdef WORDS_BIGENDIAN
#if MIN_VERSION_base(4,16,0)
    word## = narrow16Word# (byteSwap16# (word16ToWord# (indexWord16OffAddr# addr# k#)))
#else
    word## = narrow16Word# (byteSwap16# (indexWord16OffAddr# addr# k#))
#endif
#elif MIN_VERSION_base(4,16,0)
    word## = word16ToWord# (indexWord16OffAddr# addr# k#)
#else
    word## = indexWord16OffAddr# addr# k#
#endif

{-| @lookupWord32# addr index@ looks up for the @index@-th 32-bits word in
the bitmap starting at @addr@, then convert it to a 'Word#'.

The caller must make sure that:

* @ceiling (addr + (n * 32))@ is legally accessible 'GHC.Exts.Word32#'.

@since 0.4.1
-}
lookupWord32#
  :: Addr# -- ^ Bitmap address
  -> Int#  -- ^ Word index
  -> Word# -- ^ Resulting word
lookupWord32#
#ifdef WORDS_BIGENDIAN
#if MIN_VERSION_base(4,16,0)
    addr# k# = narrow32Word# (byteSwap32# (word32ToWord# (indexWord32OffAddr# addr# k#)))
#else
    addr# k# = narrow32Word# (byteSwap32# (indexWord32OffAddr# addr# k#))
#endif
#elif MIN_VERSION_base(4,16,0)
    addr# k# = word32ToWord# (indexWord32OffAddr# addr# k#)
#else
    = indexWord32OffAddr#
#endif
