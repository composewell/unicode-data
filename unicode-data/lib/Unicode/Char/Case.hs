{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Unicode.Char.Case
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Case and case mapping related functions.
--
module Unicode.Char.Case
    ( -- * Predicates
      isLowerCase
    , isLower
    , isUpperCase
    , isUpper
      -- * Case mappings
      -- $case

      -- ** Case folding mapping
    , caseFoldMapping
    , toCaseFoldString

      -- ** Lower case mapping
    , lowerCaseMapping
    , toLowerString
      -- ** Title case mapping
    , titleCaseMapping
    , toTitleString
      -- ** Upper case mapping
    , upperCaseMapping
    , toUpperString
      -- ** Types
    , Unfold(..)
    , Step(..)
    )
where

import Data.Bits (Bits(..))
import Data.Char (chr, ord)
import Data.Int (Int64)

import Unicode.Internal.Unfold
import qualified Unicode.Internal.Char.CaseFolding as C
import qualified Unicode.Internal.Char.DerivedCoreProperties as P
import qualified Unicode.Internal.Char.SpecialCasing.LowerCaseMapping as C
import qualified Unicode.Internal.Char.SpecialCasing.TitleCaseMapping as C
import qualified Unicode.Internal.Char.SpecialCasing.UpperCaseMapping as C

-- | Returns 'True' for lower-case letters.
--
-- @since 0.3.0
{-# INLINE isLowerCase #-}
isLowerCase :: Char -> Bool
isLowerCase = P.isLowercase

-- | Returns 'True' for lower-case letters.
--
-- @since 0.1.0
{-# INLINE isLower #-}
{-# DEPRECATED isLower "Use isLowerCase instead. Note that the behavior of this function does not match base:Data.Char.isLower. See Unicode.Char.Case.Compat for behavior compatible with base:Data.Char." #-}
isLower :: Char -> Bool
isLower = P.isLowercase

-- | Returns 'True' for upper-case letters.
--
-- @since 0.3.0
{-# INLINE isUpperCase #-}
isUpperCase :: Char -> Bool
isUpperCase = P.isUppercase

-- | Returns 'True' for upper-case letters.
--
-- @since 0.1.0
{-# INLINE isUpper #-}
{-# DEPRECATED isUpper "Use isUpperCase instead. Note that the behavior of this function does not match base:Data.Char.isUpper. See Unicode.Char.Case.Compat for behavior compatible with base:Data.Char." #-}
isUpper :: Char -> Bool
isUpper = P.isUppercase

-- $case
--
-- Correct case conversion rules may map one input character to two or three
-- output characters. For examples, see the documentation of 'toCaseFoldString',
-- 'toLowerString', 'toTitleString' and 'toUpperString'.
--
-- __Note:__ In some languages, case conversion is a locale- and
-- context-dependent operation. The case conversion functions in this
-- module are /not/ locale nor context sensitive.

-- [TODO] @since
-- | Returns the full /folded/ case mapping of a character.
--
-- It uses the character property @Case_Folding@.
--
-- __Note:__ @\'\\NUL\'@ requires special handling.
-- See 'toCaseFoldString' source code for an example.
{-# INLINE caseFoldMapping #-}
caseFoldMapping :: Unfold Char Char
caseFoldMapping = Unfold step inject
    where
    inject c = case C.toCasefold c of
        0 -> fromIntegral (ord c)
        k -> k

-- [TODO] @since
-- | Returns the full /lower/ case mapping of a character.
--
-- It uses the character property @Lowercase_Mapping@.
--
-- __Note:__ @\'\\NUL\'@ requires special handling.
-- See 'toLowerString' source code for an example.
{-# INLINE lowerCaseMapping #-}
lowerCaseMapping :: Unfold Char Char
lowerCaseMapping = Unfold step inject
    where
    inject c = case C.toSpecialLowerCase c of
        0 -> fromIntegral (ord c)
        k -> k

-- [TODO] @since
-- | Returns the full /title/ case mapping of a character.
--
-- It uses the character property @Titlecase_Mapping@.
--
-- __Note:__ @\'\\NUL\'@ requires special handling.
-- See 'toTitleString' source code for an example.
{-# INLINE titleCaseMapping #-}
titleCaseMapping :: Unfold Char Char
titleCaseMapping = Unfold step inject
    where
    inject c = case C.toSpecialTitleCase c of
        0 -> fromIntegral (ord c)
        k -> k

-- [TODO] @since
-- | Returns the full /upper/ case mapping of a character.
--
-- It uses the character property @Uppercase_Mapping@.
--
-- __Note:__ @\'\\NUL\'@ requires special handling.
-- See 'toUpperString' source code for an example.
{-# INLINE upperCaseMapping #-}
upperCaseMapping :: Unfold Char Char
upperCaseMapping = Unfold step inject
    where
    inject c = case C.toSpecialUpperCase c of
        0 -> fromIntegral (ord c)
        k -> k

-- [TODO] @since
-- | Convert a character to full /folded/ case if defined, else to itself.
--
-- This function is mainly useful for performing caseless (also known
-- as case insensitive) string comparisons.
--
-- A string @x@ is a caseless match for a string @y@ if and only if:
--
-- @foldMap toCaseFoldString x == foldMap toCaseFoldString y@
--
-- The result string may have more than one character, and may
-- differ from applying 'toLowerString' to the input string. For instance,
-- “&#xfb13;” (@U+FB13@ Armenian small ligature men now) is case
-- folded to the sequence “&#x574;” (@U+0574@ Armenian small letter men)
-- followed by “&#x576;” (@U+0576@ Armenian small letter now), while
-- “&#xb5;” (@U+00B5@ micro sign) is case folded to
-- “&#x3bc;” (@U+03BC@  Greek small letter mu) instead of itself.
--
-- It uses the character property @Case_Folding@.
toCaseFoldString :: Char -> String
toCaseFoldString = \case
    '\NUL' -> "\NUL"
    c      -> toList caseFoldMapping c

-- [TODO] @since
-- | Convert a character to full /lower/ case if defined, else to itself.
--
-- The result string may have more than one character. For instance,
-- “&#x130;” (@U+0130@ Latin capital letter I with dot above) maps
-- to the sequence: “i” (@U+0069@ Latin small letter I)
-- followed by “&#xa0;&#x307;” (@U+0307@ combining dot above).
--
-- It uses the character property @Lowercase_Mapping@.
--
-- See: 'Unicode.Char.Case.Compat.toLower' for /simple/ lower case conversion.
toLowerString :: Char -> String
toLowerString = \case
    '\NUL' -> "\NUL"
    c      -> toList lowerCaseMapping c

-- [TODO] @since
-- | Convert a character to full /title/ case if defined, else to itself.
--
-- The result string may have more than one character. For instance,
-- “&#xfb02;” (@U+FB02@ Latin small ligature FL) is converted to the
-- sequence: “F” (@U+0046@ Latin capital letter F) followed by
-- “l” (@U+006C@ Latin small letter L).
--
-- It uses the character property @Titlecase_Mapping@.
--
-- See: 'Unicode.Char.Case.Compat.toTitle' for /simple/ title case conversion.
toTitleString :: Char -> String
toTitleString = \case
    '\NUL' -> "\NUL"
    c      -> toList titleCaseMapping c

-- [TODO] @since
-- | Convert a character to full /upper/ case if defined, else to itself.
--
-- The result string may have more than one character. For instance,
-- the German “&#xdf;” (@U+00DF@ Eszett) maps to the
-- two-letter sequence “SS”.
--
-- It uses the character property @Uppercase_Mapping@.
--
-- See: 'Unicode.Char.Case.Compat.toUpper' for /simple/ upper case conversion.
toUpperString :: Char -> String
toUpperString = \case
    '\NUL' -> "\NUL"
    c      -> toList upperCaseMapping c

-- | Extract the next character from a raw mapping.
-- Each character is encoded on 21 bits.
--
-- **Note:** this does not work for character @\'\\NUL\'@.
{-# INLINE step #-}
step :: Int64 -> Step Int64 Char
step = \case
    0 -> Stop
    s -> Yield (chr cp) (s `shiftR` 21)
        where
            -- Mask for a single Unicode code point: (1 << 21) - 1
            mask = 0x1fffff
            cp = fromIntegral (s .&. mask)
