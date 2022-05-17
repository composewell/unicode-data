-- |
-- Module      : Unicode.Char.Numeric.Compat
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Compatibility module for numeric character property related functions.
--
module Unicode.Char.Numeric.Compat
    ( -- * Predicates
      isNumber
    ) where

import Unicode.Char.General (GeneralCategory(..), generalCategory)

-- [TODO] @since 0.3.0:; @since X.X.X: moved to compat module.
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
-- Use 'hasNumericValue' to cover those cases as well.
--
-- prop> isNumber c == Data.Char.isNumber c
isNumber :: Char -> Bool
isNumber c = case generalCategory c of
    DecimalNumber -> True
    LetterNumber  -> True
    OtherNumber   -> True
    _             -> False
