-- |
-- Module      : Unicode.Char
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental

module Unicode.Internal.Char
    ( addHexCodePoint
    , intToDigiT
    ) where

import Data.Char (ord)
import Data.Functor (($>))
import Foreign.C.String (CStringLen)
import Foreign.C.Types (CChar(..))
import Foreign.Storable (Storable(..))
import GHC.Exts (Int(..), Int#, isTrue#, quotRemInt#, (<=#), (+#), (-#))

-- | Show the code point of a character using the Unicode Standard convention:
-- hexadecimal codepoint padded with zeros if inferior to 4 characters.
--
-- It is the responsability of the caller to provide a 'CStringLen' that can hold
-- up to 6 characters from the provided index.
addHexCodePoint
    :: CStringLen -- ^ Destination ASCII string
    -> Int        -- ^ Index
    -> Char       -- ^ Character which code point will be added to the string
    -> IO Int     -- ^ New size of the string
addHexCodePoint (s, I# len) (I# i0) c
    | isTrue# (cp# <=# 0x0000f#) = showIt 3# <* pad0 0# <* pad0 1# <* pad0 2#
    | isTrue# (cp# <=# 0x000ff#) = showIt 3# <* pad0 0# <* pad0 1#
    | isTrue# (cp# <=# 0x00fff#) = showIt 3# <* pad0 0#
    | isTrue# (cp# <=# 0x0ffff#) = showIt 3#
    | isTrue# (cp# <=# 0xfffff#) = showIt 4#
    | otherwise                  = showIt 5#
    where
    !(I# cp#) = ord c
    pad0 i = pokeElemOff s (I# (i0 +# i)) (CChar 0x30)
    showIt i = go (i0 +# i) (quotRemInt# cp# 16#) $> I# (len +# i +# 1#)
    go i (# n#, d #) = do
        pokeElemOff s (I# i) (intToDigiT d)
        case n# of
            0# -> pure ()
            _  -> go (i -# 1#) (quotRemInt# n# 16#)

-- | Convert an 'Int#' in the range 0..15 to the corresponding single digit 'CChar'.
--
-- Undefined for numbers outside the 0..15 range.
intToDigiT :: Int# -> CChar
intToDigiT i = if isTrue# (i <=#  9#)
    then fromIntegral (I# (0x30# +# i))
    else fromIntegral (I# (0x37# +# i))
