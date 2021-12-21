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
    ( lookupBit64,
      lookupIntN
    ) where

#include "MachDeps.h"

import Data.Bits (finiteBitSize, popCount)
import GHC.Exts
       (Addr#, Int(..), Word(..),
        indexWordOffAddr#, indexWord8OffAddr#,
        andI#, uncheckedIShiftRL#,
        and#, word2Int#, uncheckedShiftL#)
#if MIN_VERSION_base(4,16,0)
import GHC.Exts (word8ToWord#)
#endif
#ifdef WORDS_BIGENDIAN
import GHC.Exts (byteSwap#)
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

{-| @lookupIntN addr index@ looks up for the @index@-th @8@-bits word in
the bitmap starting at @addr@, then convert it to an Int.

The caller must make sure that:

* @ceiling (addr + (n * 8))@ is legally accessible @Word8@.

@since 0.3.0
-}
lookupIntN
  :: Addr# -- ^ Bitmap address
  -> Int   -- ^ Word index
  -> Int   -- ^ Resulting word as 'Int'
lookupIntN addr# (I# index#) = I# (word2Int# word##)
  where
#if MIN_VERSION_base(4,16,0)
    word## = word8ToWord# (indexWord8OffAddr# addr# index#)
#else
    word## = indexWord8OffAddr# addr# index#
#endif
