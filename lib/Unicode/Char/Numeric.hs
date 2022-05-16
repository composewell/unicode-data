-- |
-- Module      : Unicode.Char.Numeric
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Numeric character property related functions.
--
module Unicode.Char.Numeric
    ( isNumber
    , numericValue
    , integerValue

    -- * Re-export
    , isDigit
    , isOctDigit
    , isHexDigit
    , digitToInt
    , intToDigit
    ) where

import Data.Char (digitToInt, intToDigit, isDigit, isHexDigit, isOctDigit)
import Data.Ratio (numerator, denominator)
import Unicode.Char.General (GeneralCategory(..), generalCategory)
import qualified Unicode.Internal.Char.DerivedNumericValues as V

-- | Selects Unicode numeric characters, including digits from various
-- scripts, Roman numerals, et cetera.
--
-- This function returns 'True' if its argument has one of the
-- following 'GeneralCategory's, or 'False' otherwise:
--
-- * 'DecimalNumber'
-- * 'LetterNumber'
-- * 'OtherNumber'
--
-- __Note:__ a character may have a numeric value (see 'numericValue') but return
-- 'False', because 'isNumber' only tests 'GeneralCategory':
-- some CJK characters are 'OtherLetter' and do have a numeric value.
--
-- prop> isNumber c == Data.Char.isNumber c
--
-- @since 0.3.0
isNumber :: Char -> Bool
isNumber c = case generalCategory c of
    DecimalNumber -> True
    LetterNumber  -> True
    OtherNumber   -> True
    _             -> False

-- [TODO] @since
-- | Numeric value of a character, if relevant.
--
-- __Note:__ a character may have a numeric value but return 'False' with
-- the predicate 'isNumber', because 'isNumber' only tests 'GeneralCategory':
-- some CJK characters are 'OtherLetter' and do have a numeric value.
{-# INLINE numericValue #-}
numericValue :: Char -> Maybe Rational
numericValue = V.numericValue

-- [TODO] @since
-- | Integer value of a character, if relevant.
--
-- __Note:__ a character may have a numeric value but return 'False' with
-- the predicate 'isNumber', because 'isNumber' only tests 'GeneralCategory':
-- some CJK characters are 'OtherLetter' and do have a numeric value.
integerValue :: Char -> Maybe Int
integerValue c = do
    r <- V.numericValue c
    if denominator r == 1
        then Just (fromIntegral (numerator r))
        else Nothing
