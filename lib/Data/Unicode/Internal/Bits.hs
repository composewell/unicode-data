-- Copyright 2020 Composewell Technologies and Contributers
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License.  You may obtain a copy
-- of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
-- License for the specific language governing permissions and limitations under
-- the License.

-- |
-- Module      : Data.Unicode.Internal.Bits
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
--
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, static bitmap lookup utilities

module Data.Unicode.Internal.Bits
    (
      lookupBit64
    ) where

import GHC.Exts
       (Addr#, Int(..), Word(..), indexWord64OffAddr#, and#, andI#,
        uncheckedIShiftRL#, uncheckedShiftL#)

-- | @lookup64 addr index@ looks up the bit stored at bit index @index@ using a
-- bitmap starting at the address @addr@. Looks up the 64-bit word containing
-- the bit and then the bit in that word. The caller must make sure that the
-- 64-bit word at the byte address (addr + index / 64) * 8 is legally
-- accessible memory.
--
lookupBit64 :: Addr# -> Int -> Bool
lookupBit64 addr# (I# index#) = W# (word## `and#` bitMask##) /= 0
  where
    wordIndex# = index# `uncheckedIShiftRL#` 6#
    word## = indexWord64OffAddr# addr# wordIndex#
    bitIndex# = index# `andI#` 63#
    bitMask## = 1## `uncheckedShiftL#` bitIndex#
