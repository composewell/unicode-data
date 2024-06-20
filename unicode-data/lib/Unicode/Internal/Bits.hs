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
      lookupBit,
      lookupWord8AsInt,
      lookupWord8AsInt#,
      lookupWord16AsInt,
      lookupWord16AsInt#,
      lookupWord32#,
      -- * CString
      unpackCString#
    ) where

#include "MachDeps.h"

import GHC.Exts
       (Addr#, Int(..), Int#, Word(..), Word#, indexWord8OffAddr#,
        indexWord16OffAddr#, indexWord32OffAddr#,
        and#, word2Int#, uncheckedShiftL#)
#if MIN_VERSION_base(4,16,0)
import GHC.Exts (word8ToWord#, word16ToWord#, word32ToWord#)
#endif
#ifdef WORDS_BIGENDIAN
import GHC.Exts
       (narrow16Word#, narrow32Word#,
        byteSwap16#, byteSwap32#)
#endif

#if MIN_VERSION_base(4,15,0)
import GHC.Exts (unpackCString#)
#else
import GHC.CString (unpackCString#)
#endif

-- TODO: remove?
-- {- | @lookupBit addr index@ looks up the bit stored at bit index @index@ using
-- a bitmap starting at the address @addr@. Looks up the word containing the bit
-- and then the bit in that word. The caller must make sure that the word at the
-- byte address @(addr + index / wfbs)@, where @wfbs@ is the finite bit size of a
-- word, is legally accessible memory.
-- -}
-- lookupBit :: Addr# -> Int -> Bool
-- lookupBit addr# (I# index#) = W# (word## `and#` bitMask##) /= 0
--   where
--     !fbs@(I# fbs#) = finiteBitSize (0 :: Word) - 1
--     !(I# log2Fbs) = case fbs of
--       31 -> 5
--       63 -> 6
--       _  -> popCount fbs -- this is a really weird architecture

--     wordIndex# = index# `uncheckedIShiftRL#` log2Fbs
-- #ifdef WORDS_BIGENDIAN
--     word## = byteSwap# (indexWordOffAddr# addr# wordIndex#)
-- #else
--     word## = indexWordOffAddr# addr# wordIndex#
-- #endif
--     -- x % 2^n = x & (2^n - 1)
--     bitIndex# = index# `andI#` fbs#
--     bitMask## = 1## `uncheckedShiftL#` bitIndex#

{- | @lookupBit addr byteIndex bitIndex@ looks up the bit stored in the byte
at index @byteIndex@ at the bit index @bitIndex@ using a bitmap starting at the
address @addr@. The caller must make sure that the byte at address
@(addr + byteIndex)@ is legally accessible memory.
-}
lookupBit :: Addr# -> Int -> Int -> Bool
lookupBit addr# (I# byteIndex#) (I# bitIndex#) =
    W# (word## `and#` bitMask##) /= 0
  where
#if MIN_VERSION_base(4,16,0)
    word## = word8ToWord# (indexWord8OffAddr# addr# byteIndex#)
#else
    word## = indexWord8OffAddr# addr# byteIndex#
#endif
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
lookupWord8AsInt addr# (I# index#) = I# (lookupWord8AsInt# addr# index#)

lookupWord8AsInt#
  :: Addr# -- ^ Bitmap address
  -> Int#  -- ^ Word index
  -> Int#  -- ^ Resulting word as 'Int'
lookupWord8AsInt# addr# index# = word2Int# word##
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
lookupWord16AsInt addr# (I# k#) = I# (lookupWord16AsInt# addr# k#)

lookupWord16AsInt#
  :: Addr# -- ^ Bitmap address
  -> Int#  -- ^ Word index
  -> Int#  -- ^ Resulting word as `Int`
lookupWord16AsInt# addr# k# = word2Int# word##
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
