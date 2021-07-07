-- |
-- Module      : Unicode.Char.General
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- General character property related functions.
--
module Unicode.Char.General
    (
    -- * Character Properties
      isLetter
    , isSpace

    -- * Korean Hangul Characters
    -- | The Hangul script used in the Korean writing system consists of
    -- individual consonant and vowel letters (jamo) that are visually combined
    -- into square display cells to form entire syllable  blocks.  Hangul
    -- syllables  may  be  encoded  directly  as  precomposed  combinations of
    -- individual jamo or as decomposed sequences of conjoining jamo. Modern
    -- Hangul syllable blocks can be expressed with either two or three jamo,
    -- either in the  form  consonant + vowel  or  in  the  form  consonant +
    -- vowel + consonant. The leading consonant is represented as L, the vowel
    -- as V and the trailing consonant as T.
    --
    -- The Unicode Standard contains both a large set of precomposed modern
    -- Hangul syllables and a set of conjoining Hangul jamo, which can be used
    -- to encode archaic Korean syllable blocks as well as modern Korean
    -- syllable blocks.
    --
    -- Hangul characters can be composed or decomposed algorithmically instead
    -- of via mappings.  These APIs are used mainly for Unicode normalization
    -- of Hangul text.
    --
    -- Please refer to the following resources for more information:
    --
    -- * The @Hangul@ section of the @East Asia@ chapter of the [Unicode Standard](https://www.unicode.org/versions/latest)
    -- * Conformance chapter of the [Unicode Standard](https://www.unicode.org/versions/latest)
    -- * [Unicode® Standard Annex #15 - Unicode Normalization Forms](https://www.unicode.org/reports/tr15)
    -- * UCD file @HangulSyllableType.txt@
    -- * https://en.wikipedia.org/wiki/Hangul_Jamo_(Unicode_block)
    -- * https://en.wikipedia.org/wiki/List_of_Hangul_jamo

    -- ** Conjoining Jamo
    -- | Jamo L, V and T letters.
    , isJamo
    , jamoNCount

    -- *** Jamo Leading (L)
    , jamoLFirst
    , jamoLIndex
    , jamoLLast

    -- *** Jamo Vowel (V)
    , jamoVFirst
    , jamoVCount
    , jamoVIndex
    , jamoVLast

    -- *** Jamo Trailing (T)
    , jamoTFirst
    , jamoTCount
    , jamoTIndex
    , jamoTLast

    -- ** Hangul Syllables
    -- | Precomposed Hangul syllables.
    , hangulFirst
    , hangulLast
    , isHangul
    , isHangulLV
    )
where

import Control.Exception (assert)
import Data.Char (ord)
import Unicode.Internal.Division (quotRem28)

import qualified Unicode.Internal.Char.DerivedCoreProperties as P
import qualified Unicode.Internal.Char.PropList as P

-- | Returns 'True' for alphabetic Unicode characters (lower-case, upper-case
-- and title-case letters, plus letters of caseless scripts and modifiers
-- letters).
--
-- prop> isLetter c == Data.Char.isLetter c
--
{-# INLINE isLetter #-}
isLetter :: Char -> Bool
isLetter = P.isAlphabetic

-- | Returns 'True' for any whitespace characters, and the control
-- characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.
--
-- prop> isSpace c == Data.Char.isSpace c
--
{-# INLINE isSpace #-}
isSpace :: Char -> Bool
isSpace = P.isWhite_Space

-------------------------------------------------------------------------------
-- Korean Hangul
-------------------------------------------------------------------------------

-- jamo leading
jamoLFirst, jamoLCount, jamoLLast :: Int

-- | First leading consonant jamo.
jamoLFirst  = 0x1100

-- | Total count of leading consonant jamo.
jamoLCount = 19

-- | Last leading consonant jamo.
jamoLLast = jamoLFirst + jamoLCount - 1

-- jamo vowel
jamoVFirst, jamoVCount, jamoVLast :: Int

-- | First vowel jamo.
jamoVFirst  = 0x1161

-- | Total count of vowel jamo.
jamoVCount = 21

-- | Last vowel jamo.
jamoVLast = jamoVFirst + jamoVCount - 1

-- jamo trailing
jamoTFirst, jamoTCount :: Int

-- | The first trailing consonant jamo.
--
-- Note that 'jamoTFirst' does not represent a valid T, it represents a missing
-- T i.e. LV without a T. See comments under 'jamoTIndex' .
jamoTFirst  = 0x11a7

-- | Total count of trailing consonant jamo.
jamoTCount = 28

-- | Last trailing consonant jamo.
jamoTLast :: Int
jamoTLast = jamoTFirst + jamoTCount - 1

-- | Total count of all jamo characters.
--
-- @jamoNCount = jamoVCount * jamoTCount@
jamoNCount :: Int
jamoNCount = 588

-- hangul
hangulFirst, hangulLast :: Int

-- | Codepoint of the first pre-composed Hangul character.
hangulFirst = 0xac00

-- | Codepoint of the last Hangul character.
hangulLast = hangulFirst + jamoLCount * jamoVCount * jamoTCount - 1

-- | Determine if the given character is a precomposed Hangul syllable.
isHangul :: Char -> Bool
isHangul c = n >= hangulFirst && n <= hangulLast
    where n = ord c

-- | Determine if the given character is a Hangul LV syllable.
isHangulLV :: Char -> Bool
isHangulLV c = assert (jamoTCount == 28)
    snd (quotRem28 (ord c - hangulFirst)) == 0

-- | Determine whether a character is a jamo L, V or T character.
isJamo :: Char -> Bool
isJamo c = n >= jamoLFirst && n <= jamoTLast
    where n = ord c

-- | Given a Unicode character, if it is a leading jamo, return its index in
-- the list of leading jamo consonants, otherwise return 'Nothing'.
jamoLIndex :: Char -> Maybe Int
jamoLIndex c
  | index >= 0 && index < jamoLCount = Just index
  | otherwise = Nothing
    where index = ord c - jamoLFirst

-- | Given a Unicode character, if it is a vowel jamo, return its index in the
-- list of vowel jamo, otherwise return 'Nothing'.
jamoVIndex :: Char -> Maybe Int
jamoVIndex c
  | index >= 0 && index < jamoVCount = Just index
  | otherwise = Nothing
    where index = ord c - jamoVFirst

-- | Given a Unicode character, if it is a trailing jamo consonant, return its
-- index in the list of trailing jamo consonants, otherwise return 'Nothing'.
--
-- Note that index 0 is not a valid index for a trailing consonant. Index 0
-- corresponds to an LV syllable, without a T.  See "Hangul Syllable
-- Decomposition" in the Conformance chapter of the Unicode standard for more
-- details.
--
jamoTIndex :: Char -> Maybe Int
jamoTIndex c
  | index > 0 && index < jamoTCount = Just index
  | otherwise = Nothing
    where index = ord c - jamoTFirst
