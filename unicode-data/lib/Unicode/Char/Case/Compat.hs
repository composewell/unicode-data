-- |
-- Module      : Unicode.Char.Case.Compat
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Compatibility module for case and case mapping related functions.
--
-- The functions of this module are drop-in replacement for those in "Data.Char".
-- They are similar but not identical to some functions in "Unicode.Char.Case",
-- therefore they are placed in a separate module in order to avoid ambiguity.
--
module Unicode.Char.Case.Compat
    ( -- * Predicates
      isUpper
    , isLower
      -- * Case conversion
    , toUpper
    , toLower
    , toTitle
    ) where

import Data.Char (ord)
import qualified Unicode.Internal.Char.UnicodeData.GeneralCategory as UC
import qualified Unicode.Internal.Char.UnicodeData.SimpleLowerCaseMapping as C
import qualified Unicode.Internal.Char.UnicodeData.SimpleTitleCaseMapping as C
import qualified Unicode.Internal.Char.UnicodeData.SimpleUpperCaseMapping as C

-- | Selects upper-case or title-case alphabetic Unicode characters (letters).
-- Title case is used by a small number of letter ligatures like the
-- single-character form of /Lj/.
--
-- It matches characters with general category
-- 'Unicode.Char.General.UppercaseLetter' and
-- 'Unicode.Char.General.TitlecaseLetter'.
--
-- See: 'Unicode.Char.Case.isUpperCase' for the /full upper/ case predicate.
--
-- prop> isUpper c == Data.Char.isUpper c
--
-- @since 0.3.0
isUpper :: Char -> Bool
isUpper c =
    -- NOTE: The guard constant is updated at each Unicode revision.
    --       It must be < 0x40000 to be accepted by generalCategoryPlanes0To3.
    cp <= UC.MaxIsUpper &&
    case UC.generalCategoryPlanes0To3 cp of
        UC.UppercaseLetter -> True
        UC.TitlecaseLetter -> True
        _                  -> False
    where cp = ord c

-- | Selects lower-case alphabetic Unicode characters (letters).
--
-- It matches characters with general category
-- 'Unicode.Char.General.LowercaseLetter'.
--
-- See: 'Unicode.Char.Case.isLowerCase' for the /full/ lower case predicate.
--
-- prop> isLower c == Data.Char.isLower c
--
-- @since 0.3.0
isLower :: Char -> Bool
isLower c =
    -- NOTE: The guard constant is updated at each Unicode revision.
    --       It must be < 0x40000 to be accepted by generalCategoryPlanes0To3.
    cp <= UC.MaxIsLower &&
    case UC.generalCategoryPlanes0To3 cp of
        UC.LowercaseLetter -> True
        _                  -> False
    where cp = ord c

-- | Convert a letter to the corresponding upper-case letter, if any.
-- Any other character is returned unchanged.
--
-- It uses the character property @Simple_Uppercase_Mapping@.
--
-- See: 'Unicode.Char.Case.upperCaseMapping' and
-- 'Unicode.Char.Case.toUpperString' for /full/ upper case conversion.
--
-- prop> toUpper c == Data.Char.toUpper c
--
-- @since 0.3.0
{-# INLINE toUpper #-}
toUpper :: Char -> Char
toUpper = C.toSimpleUpperCase

-- | Convert a letter to the corresponding lower-case letter, if any.
-- Any other character is returned unchanged.
--
-- It uses the character property @Simple_Lowercase_Mapping@.
--
-- See: 'Unicode.Char.Case.lowerCaseMapping' and
-- 'Unicode.Char.Case.toLowerString' for /full/ lower case conversion.
--
-- prop> toLower c == Data.Char.toLower c
--
-- @since 0.3.0
{-# INLINE toLower #-}
toLower :: Char -> Char
toLower = C.toSimpleLowerCase

-- | Convert a letter to the corresponding title-case or upper-case letter,
-- if any. (Title case differs from upper case only for a small number of
-- ligature letters.) Any other character is returned unchanged.
--
-- It uses the character property @Simple_Titlecase_Mapping@.
--
-- See: 'Unicode.Char.Case.titleCaseMapping' and
-- 'Unicode.Char.Case.toTitleString' for /full/ title case conversion.
--
-- prop> toTitle c == Data.Char.toTitle c
--
-- @since 0.3.0
{-# INLINE toTitle #-}
toTitle :: Char -> Char
toTitle = C.toSimpleTitleCase
