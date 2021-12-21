-- |
-- Module      : Unicode.Char.General.Compat
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Compatibility module for general character property related functions.
--
-- The functions of this module are drop-in replacement for those in "Data.Char".
-- They are similar but not identical to some functions in "Unicode.Char.General",
-- therefore they are placed in a separate module in order to avoid ambiguity.
--
module Unicode.Char.General.Compat
    ( isAlpha
    , isLetter
    , isSpace
    ) where

import Unicode.Char.General (GeneralCategory(..), generalCategory)

-- | Same as 'isLetter'.
--
-- @since 0.3.0
{-# INLINE isAlpha #-}
isAlpha :: Char -> Bool
isAlpha = isLetter

{-| Selects alphabetic Unicode characters (lower-case, upper-case and title-case
letters, plus letters of caseless scripts and modifiers letters).

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

* 'UppercaseLetter'
* 'LowercaseLetter'
* 'TitlecaseLetter'
* 'ModifierLetter'
* 'OtherLetter'

__Note:__ this function is /not/ equivalent to 'Unicode.Char.General.isAlphabetic'.
See the description of 'Unicode.Char.General.isAlphabetic' for further details.

prop> isLetter c == Data.Char.isLetter c

@since 0.3.0
-}
{-# INLINE isLetter #-}
isLetter :: Char -> Bool
isLetter c = case generalCategory c of
    UppercaseLetter -> True
    LowercaseLetter -> True
    TitlecaseLetter -> True
    ModifierLetter  -> True
    OtherLetter     -> True
    _               -> False

{-| Selects Unicode space characters (general category 'Space'),
and the control characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.

__Note:__ 'isSpace' is /not/ equivalent to 'Unicode.Char.General.isWhiteSpace'.
'Unicode.Char.General.isWhiteSpace' selects the same characters from 'isSpace'
plus the following:

* @U+0085@ NEXT LINE (NEL)
* @U+2028@ LINE SEPARATOR
* @U+2029@ PARAGRAPH SEPARATOR

prop> isSpace c == Data.Char.isSpace c

@since 0.3.0
-}
isSpace :: Char -> Bool
isSpace '\t' = True
isSpace '\n' = True
isSpace '\v' = True
isSpace '\f' = True
isSpace '\r' = True
isSpace c = case generalCategory c of
    Space -> True
    _     -> False
