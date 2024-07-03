-- |
-- Module      : Unicode.Char.Numeric
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Numeric character property related functions.
--
-- @since 0.3.0
module Unicode.Char.Numeric
    ( -- * Predicates
      isNumeric

      -- * Numeric values
    , numericValue
    , integerValue

      -- * Single digit characters
    , intToDigiT

      -- * Re-export from @base@
    , isDigit
    , isOctDigit
    , isHexDigit
    , digitToInt
    , intToDigit
    ) where

import Data.Char (digitToInt, intToDigit, isDigit, isHexDigit, isOctDigit)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Ratio (denominator, numerator)
import GHC.Exts (Char (..), Int (..), chr#, isTrue#, (+#), (<=#), (>=#))

import qualified Unicode.Internal.Char.DerivedNumericValues as V

-- $setup
-- >>> import Data.Int (Int32, Int64)

-- | Selects Unicode character with a numeric value.
--
-- __Note:__ a character may have a numeric value but return 'False' with
-- the predicate 'Unicode.Char.Numeric.Compat.isNumber', because
-- 'Unicode.Char.Numeric.Compat.isNumber' only tests
-- 'Unicode.Char.General.GeneralCategory': some CJK characters are
-- 'Unicode.Char.General.OtherLetter' and do have a numeric value.
--
-- prop> isNumeric c == isJust (numericValue c)
--
-- @since 0.3.1
{-# INLINE isNumeric #-}
isNumeric :: Char -> Bool
isNumeric = isJust . V.numericValue

-- | Numeric value of a character, if relevant.
--
-- __Note:__ a character may have a numeric value but return 'False' with
-- the predicate 'Unicode.Char.Numeric.Compat.isNumber', because
-- 'Unicode.Char.Numeric.Compat.isNumber' only tests
-- 'Unicode.Char.General.GeneralCategory': some CJK characters are
-- 'Unicode.Char.General.OtherLetter' and do have a numeric value.
--
-- @since 0.3.1
{-# INLINE numericValue #-}
numericValue :: Char -> Maybe Rational
numericValue = V.numericValue

-- | Integer value of a character, if relevant.
--
-- This is a special case of 'numericValue'.
--
-- __Warning:__ There is a risk of /integer overflow/ depending of the chosen
-- concrete return type. As of Unicode 15.1 the results range from 0 to 1e16.
--
-- >>> minimum [v | v@Just{} <- integerValue <$> [minBound..]] :: Maybe Integer
-- Just 0
-- >>> maximum (integerValue <$> [minBound..]) :: Maybe Integer
-- Just 10000000000000000
-- >>> integerValue '\x4EAC' :: Maybe Int64 -- OK
-- Just 10000000000000000
-- >>> integerValue '\x4EAC' :: Maybe Int32 -- Will overflow!
-- Just 1874919424
--
-- Therefore it is advised to use: @'integerValue' \@'Int64'@.
--
-- __Note:__ A character may have a numeric value but return 'False' with
-- the predicate 'Unicode.Char.Numeric.Compat.isNumber', because
-- 'Unicode.Char.Numeric.Compat.isNumber' only tests
-- 'Unicode.Char.General.GeneralCategory': some CJK characters are
-- 'Unicode.Char.General.OtherLetter' and do have a numeric value.
--
-- @since 0.3.1
{-# INLINE integerValue #-}
{-# SPECIALIZE integerValue :: Char -> Maybe Integer #-}
{-# SPECIALIZE integerValue :: Char -> Maybe Int64   #-}
{-# SPECIALIZE integerValue :: Char -> Maybe Int     #-}
integerValue :: (Integral a) => Char -> Maybe a
integerValue c = do
    r <- V.numericValue c
    if denominator r == 1
        then Just (fromInteger (numerator r))
        else Nothing

-- | Same a 'intToDigit', but with upper case.
--
-- >>> intToDigiT <$> [0..15]
-- "0123456789ABCDEF"
--
-- @since 0.6.0
intToDigiT :: Int -> Char
intToDigiT (I# i)
    | isTrue# (i >=# 0#)  && isTrue# (i <=#  9#) = C# (chr# (0x30# +# i))
    | isTrue# (i >=# 10#) && isTrue# (i <=# 15#) = C# (chr# (0x37# +# i))
    | otherwise =  errorWithoutStackTrace
        ("Unicode.Char.Numeric.intToDigiT: not a digit " ++ show (I# i))
