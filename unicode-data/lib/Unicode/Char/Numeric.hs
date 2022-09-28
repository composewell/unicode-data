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

-- | Integer value of a character, if relevant.
--
-- This is a special case of 'numericValue'.
--
-- __Note:__ a character may have a numeric value but return 'False' with
-- the predicate 'Unicode.Char.Numeric.Compat.isNumber', because
-- 'Unicode.Char.Numeric.Compat.isNumber' only tests
-- 'Unicode.Char.General.GeneralCategory': some CJK characters are
-- 'Unicode.Char.General.OtherLetter' and do have a numeric value.
--
-- @since 0.3.1
integerValue :: Char -> Maybe Int
integerValue c = do
    r <- V.numericValue c
    if denominator r == 1
        then Just (fromIntegral (numerator r))
        else Nothing
