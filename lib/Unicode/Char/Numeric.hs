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

    -- * Re-export
    , isDigit
    , isOctDigit
    , isHexDigit
    , digitToInt
    , intToDigit
    ) where

import Data.Char (digitToInt, intToDigit, isDigit, isHexDigit, isOctDigit)
import Unicode.Char.General (GeneralCategory(..), generalCategory)

{-| Selects Unicode numeric characters, including digits from various
scripts, Roman numerals, et cetera.

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

* 'DecimalNumber'
* 'LetterNumber'
* 'OtherNumber'

prop> isNumber c == Data.Char.isNumber c

@since 0.3.0
-}
isNumber :: Char -> Bool
isNumber c = case generalCategory c of
    DecimalNumber -> True
    LetterNumber  -> True
    OtherNumber   -> True
    _             -> False
