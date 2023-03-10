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
    , isNumber

      -- * Numeric values
    , numericValue
    , integerValue

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
import Data.Ratio (numerator, denominator)
import qualified Unicode.Char.Numeric.Compat as Compat
import qualified Unicode.Internal.Char.DerivedNumericValues as V

-- | Selects Unicode numeric characters, including digits from various
-- scripts, Roman numerals, et cetera.
--
-- This function returns 'True' if its argument has one of the
-- following 'Unicode.Char.General.GeneralCategory's, or 'False' otherwise:
--
-- * 'Unicode.Char.General.DecimalNumber'
-- * 'Unicode.Char.General.LetterNumber'
-- * 'Unicode.Char.General.OtherNumber'
--
-- __Note:__ a character may have a numeric value (see 'numericValue') but return
-- 'False', because 'isNumber' only tests 'Unicode.Char.General.GeneralCategory':
-- some CJK characters are 'Unicode.Char.General.OtherLetter' and do have a
-- numeric value. Use 'isNumeric' to cover those cases as well.
--
-- prop> isNumber c == Data.Char.isNumber c
--
-- @since 0.3.0
{-# DEPRECATED isNumber "Use Unicode.Char.Numeric.Compat.isNumber instead. This function will be a synonym for isNumeric in a future release. See Unicode.Char.Numeric.Compat for behavior compatible with base:Data.Char." #-}
{-# INLINE isNumber #-}
isNumber :: Char -> Bool
isNumber = Compat.isNumber

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

-- $setup
-- >>> import Data.Int (Int32, Int64)

-- | Integer value of a character, if relevant.
--
-- This is a special case of 'numericValue'.
--
-- __Warning:__ There is a risk of /integer overflow/ depending of the chosen
-- concrete return type. As of Unicode 15.0 the results range from 0 to 1e12.
--
-- >>> integerValue '\x5146' :: Maybe Int64 -- OK
-- Just 1000000000000
-- >>> integerValue '\x5146' :: Maybe Int32 -- Will overflow!
-- Just (-727379968)
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
