{-# LANGUAGE LambdaCase #-}

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

import Data.Char (ord)
import qualified Unicode.Internal.Char.UnicodeData.GeneralCategory as UC

-- | Same as 'isLetter'.
--
-- @since 0.3.0
{-# INLINE isAlpha #-}
isAlpha :: Char -> Bool
isAlpha = isLetter

{-| Selects alphabetic Unicode characters (lower-case, upper-case and title-case
letters, plus letters of caseless scripts and modifiers letters).

This function returns 'True' if its argument has one of the
following 'Unicode.Char.General.GeneralCategory's, or 'False' otherwise:

* 'Unicode.Char.General.UppercaseLetter'
* 'Unicode.Char.General.LowercaseLetter'
* 'Unicode.Char.General.TitlecaseLetter'
* 'Unicode.Char.General.ModifierLetter'
* 'Unicode.Char.General.OtherLetter'

__Note:__ this function is /not/ equivalent to 'Unicode.Char.General.isAlphabetic'.
See the description of 'Unicode.Char.General.isAlphabetic' for further details.

prop> isLetter c == Data.Char.isLetter c

@since 0.3.0
-}
{-# INLINE isLetter #-}
isLetter :: Char -> Bool
isLetter c =
    let !cp = ord c
    -- NOTE: The guard constant is updated at each Unicode revision.
    --       It must be < 0x40000 to be accepted by generalCategoryPlanes0To3.
    in cp <= UC.MaxIsLetter &&
        let !gc = UC.generalCategoryPlanes0To3 cp
        in gc <= UC.OtherLetter
    -- Use the following in case the previous code is not valid anymore:
    -- UC.generalCategory c <= UC.OtherLetter

{-| Selects Unicode space characters (general category
'Unicode.Char.General.Space'), and the control characters
@\\t@, @\\n@, @\\r@, @\\f@, @\\v@.

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
-- NOTE: The guard constant is updated at each Unicode revision.
--       It must be < 0x40000 to be accepted by generalCategoryPlanes0To3.
isSpace c = cp <= UC.MaxIsSpace && case c of
    '\t' -> True
    '\n' -> True
    '\v' -> True
    '\f' -> True
    '\r' -> True
    _    -> UC.generalCategoryPlanes0To3 cp == UC.Space
    where cp = ord c
