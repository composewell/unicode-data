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

import Data.Char (ord)
import qualified Unicode.Internal.Char.UnicodeData.GeneralCategory as UC

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
isNumber c =
    -- NOTE: The guard constant is updated at each Unicode revision.
    --       It must be < 0x40000 to be accepted by generalCategoryPlanes0To3.
    cp <= UC.MaxIsNumber &&
    case UC.generalCategoryPlanes0To3 cp of
        UC.DecimalNumber -> True
        UC.LetterNumber  -> True
        UC.OtherNumber   -> True
        _                -> False
    where cp = ord c
