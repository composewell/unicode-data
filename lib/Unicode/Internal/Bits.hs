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
    (
      lookupBit64,
      lookupIntN
    ) where

import Data.Bits (finiteBitSize, popCount)
import GHC.Exts
       (Addr#, Int(..), Word(..),
        indexWordOffAddr#,
        andI#, quotRemInt#, int2Word#, (*#), (-#), (+#), (<=#), uncheckedIShiftL#, uncheckedIShiftRL#, isTrue#,
        and#, or#, word2Int#, uncheckedShiftL#, uncheckedShiftRL#)

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
    word## = indexWordOffAddr# addr# wordIndex#
    bitIndex# = index# `andI#` fbs#
    bitMask## = 1## `uncheckedShiftL#` bitIndex#

{-| @lookupIntN addr bs index@ looks up for the @index@-th @bs@-bits word in the bitmap
the bitmap starting at @addr@, then convert it to an Int.

The caller must make sure that:

* @bs@ if strictly inferior to the 'Word' bit size (@wbs@)
  and that @ceiling (addr + (n * bs) \/ wbs)@ is legally accessible 'Word'.
* @bs * index@ can be represented by an 'Int'.
-}
lookupIntN
  :: Addr# -- ^ Bitmap address
  -> Int   -- ^ Word bit size
  -> Int   -- ^ Word index
  -> Int   -- ^ Resulting word as 'Int'
lookupIntN addr# (I# bs#) (I# index#) = I# (word2Int# wordn##)
  where
    !(I# wbs#) = finiteBitSize (0 :: Word)
    !(# q#, r# #) = quotRemInt# (index# *# bs#) wbs#
    mask## = int2Word# ((1# `uncheckedIShiftL#` bs#) -# 1#)
    word## = indexWordOffAddr# addr# q#
    wordn## =
      let w## = word## `uncheckedShiftRL#` r#
          d# = wbs# -# r# -- bits used in w##
      in if isTrue# (bs# <=# d#)
        -- Data within a word
        then w## `and#` mask##
        -- Data across 2 words
        else let word'## = indexWordOffAddr# addr# (q# +# 1#)
                 w'## = word'## `uncheckedShiftL#` d#
             in (w## `or#` w'##) `and#` mask##
