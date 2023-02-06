-- |
-- Module      : Unicode.Char.Numeric.Compat
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Compatibility module for numeric character property related functions.
--
-- @since 0.3.1
--
module Unicode.Char.Numeric.Compat
    ( -- * Predicates
      isNumber
    ) where

import Unicode.Char.General (GeneralCategory(..), generalCategory)

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
-- __Note:__ a character may have a numeric value (see
-- 'Unicode.Char.Numeric.numericValue') but return
-- 'False', because 'isNumber' only tests 'GeneralCategory':
-- some CJK characters are 'OtherLetter' and do have a numeric value.
-- Use 'Unicode.Char.Numeric.isNumeric' to cover those cases as well.
--
-- prop> isNumber c == Data.Char.isNumber c
--
-- @since 0.3.1 moved to Compat module.
--
-- @since 0.3.0
isNumber :: Char -> Bool
isNumber c = case generalCategory c of
    DecimalNumber -> True
    LetterNumber  -> True
    OtherNumber   -> True
    _             -> False
