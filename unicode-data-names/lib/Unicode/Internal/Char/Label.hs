-- |
-- Module      : Unicode.Char
-- Copyright   : (c) 2024 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental

module Unicode.Internal.Char.Label
    ( label
    , addHexCodePoint
    , intToDigiT
    ) where

import Data.Char (ord)
import Data.Functor (($>))
import Foreign.C.String (CString, CStringLen)
import Foreign.C.Types (CChar (..))
import Foreign.Marshal (allocaArray, copyArray)
import Foreign.Storable (Storable (..))
import GHC.Exts (Int (..), Int#, Ptr (..), isTrue#, quotRemInt#, (+#), (-#), (<=#))
import Unicode.Char.General (CodePointType (..), codePointType)

-- | Returns the label of a code point if it has no character name, otherwise
-- returns @\"UNDEFINED\"@.
--
-- See subsection
-- [“Code Point Labels”](https://www.unicode.org/versions/Unicode15.0.0/ch04.pdf#G135248)
-- in section 4.8 “Name” of the Unicode Standard.
--
-- @since 0.4.0
label :: Char -> IO CStringLen
label c = case codePointType c of
    ControlType      -> mkLabel 8#  "control-"#
    PrivateUseType   -> mkLabel 12# "private-use-"#
    SurrogateType    -> mkLabel 10# "surrogate-"#
    NoncharacterType -> mkLabel 13# "noncharacter-"#
    ReservedType     -> mkLabel 9#  "reserved-"#
    _                -> pure (Ptr "UNDEFINED"#, 9)

    where

    mkLabel len s0 = allocaArray (I# len + 6) $ \s -> do
        copyArray s (Ptr s0) (I# len)
        len' <- addHexCodePoint s len len c
        pure (s, len')

-- | Appned the code point of a character using the Unicode Standard convention:
-- hexadecimal codepoint padded with zeros if inferior to 4 characters.
--
-- It is the responsability of the caller to provide a 'CString' that can hold
-- up to 6 characters from the provided index.
addHexCodePoint
    :: CString -- ^ Destination ASCII string
    -> Int#    -- ^ String length
    -> Int#    -- ^ Index
    -> Char    -- ^ Character which code point will be added to the string
    -> IO Int  -- ^ New size of the string
addHexCodePoint s len i0 c
    | isTrue# (cp# <=# 0x0000f#) = prependAt 3# <* pad0 0# <* pad0 1# <* pad0 2#
    | isTrue# (cp# <=# 0x000ff#) = prependAt 3# <* pad0 0# <* pad0 1#
    | isTrue# (cp# <=# 0x00fff#) = prependAt 3# <* pad0 0#
    | isTrue# (cp# <=# 0x0ffff#) = prependAt 3#
    | isTrue# (cp# <=# 0xfffff#) = prependAt 4#
    | otherwise                  = prependAt 5#
    where
    !(I# cp#) = ord c
    pad0 i = pokeElemOff s (I# (i0 +# i)) (CChar 0x30)
    prependAt i = go (i0 +# i) (quotRemInt# cp# 16#) $> I# (len +# i +# 1#)
    go i (# n#, d #) = do
        pokeElemOff s (I# i) (intToDigiT d)
        case n# of
            0# -> pure ()
            _  -> go (i -# 1#) (quotRemInt# n# 16#)

-- | Convert an 'Int#' in the range 0..15 to the corresponding single digit
-- 'CChar' in upper case.
--
-- Undefined for numbers outside the 0..15 range.
intToDigiT :: Int# -> CChar
intToDigiT i = if isTrue# (i <=# 9#)
    then fromIntegral (I# (0x30# +# i))
    else fromIntegral (I# (0x37# +# i))
