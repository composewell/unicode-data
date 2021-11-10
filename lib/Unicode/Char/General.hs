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
    -- * Unicode general categories
      GeneralCategory(..)
    , generalCategoryAbbr
    , generalCategory

    -- * Character classification
    , isAlpha
    , isAlphabetic
    , isAlphaNum
    , isControl
    , isLetter
    , isMark
    , isNumber
    , isPrint
    , isPunctuation
    , isSeparator
    , isSpace
    , isSymbol
    , isWhiteSpace

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
import Data.Ix (Ix)
import Unicode.Internal.Division (quotRem28)

import qualified Unicode.Internal.Char.DerivedCoreProperties as P
import qualified Unicode.Internal.Char.PropList as P
import qualified Unicode.Internal.Char.UnicodeData.GeneralCategory as UC

{-| Unicode General Categories.

These classes are defined in the
[Unicode Character Database](http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table),
part of the Unicode standard

__Note:__ the classes must be in the same order they are listed in the Unicode Standard,
because some functions (e.g. 'generalCategory') rely on the 'Enum' instance.
-}
data GeneralCategory
  -- L: Letter
  = UppercaseLetter       -- ^ @Lu@: Letter, Uppercase
  | LowercaseLetter       -- ^ @Ll@: Letter, Lowercase
  | TitlecaseLetter       -- ^ @Lt@: Letter, Titlecase
  | ModifierLetter        -- ^ @Lm@: Letter, Modifier
  | OtherLetter           -- ^ @Lo@: Letter, Other

  -- M: Mark
  | NonSpacingMark        -- ^ @Mn@: Mark, Non-Spacing
  | SpacingCombiningMark  -- ^ @Mc@: Mark, Spacing Combining
  | EnclosingMark         -- ^ @Me@: Mark, Enclosing

  -- N: Number
  | DecimalNumber         -- ^ @Nd@: Number, Decimal
  | LetterNumber          -- ^ @Nl@: Number, Letter
  | OtherNumber           -- ^ @No@: Number, Other

  -- P: Punctuation
  | ConnectorPunctuation  -- ^ @Pc@: Punctuation, Connector
  | DashPunctuation       -- ^ @Pd@: Punctuation, Dash
  | OpenPunctuation       -- ^ @Ps@: Punctuation, Open
  | ClosePunctuation      -- ^ @Pe@: Punctuation, Close
  | InitialQuote          -- ^ @Pi@: Punctuation, Initial quote
  | FinalQuote            -- ^ @Pf@: Punctuation, Final quote
  | OtherPunctuation      -- ^ @Po@: Punctuation, Other

  -- S: Symbol
  | MathSymbol            -- ^ @Sm@: Symbol, Math
  | CurrencySymbol        -- ^ @Sc@: Symbol, Currency
  | ModifierSymbol        -- ^ @Sk@: Symbol, Modifier
  | OtherSymbol           -- ^ @So@: Symbol, Other

  -- Z: Separator
  | Space                 -- ^ @Zs@: Separator, Space
  | LineSeparator         -- ^ @Zl@: Separator, Line
  | ParagraphSeparator    -- ^ @Zp@: Separator, Paragraph

  -- C: Other
  | Control               -- ^ @Cc@: Other, Control
  | Format                -- ^ @Cf@: Other, Format
  | Surrogate             -- ^ @Cs@: Other, Surrogate
  | PrivateUse            -- ^ @Co@: Other, Private Use
  | NotAssigned           -- ^ @Cn@: Other, Not Assigned
  deriving ( Show
           , Eq
           , Ord
           , Enum
           , Bounded
           , Ix
           )

-- | Abbreviation of 'GeneralCategory' used in the Unicode standard.
generalCategoryAbbr :: GeneralCategory -> String
generalCategoryAbbr UppercaseLetter      = "Lu"
generalCategoryAbbr LowercaseLetter      = "Ll"
generalCategoryAbbr TitlecaseLetter      = "Lt"
generalCategoryAbbr ModifierLetter       = "Lm"
generalCategoryAbbr OtherLetter          = "Lo"
generalCategoryAbbr NonSpacingMark       = "Mn"
generalCategoryAbbr SpacingCombiningMark = "Mc"
generalCategoryAbbr EnclosingMark        = "Me"
generalCategoryAbbr DecimalNumber        = "Nd"
generalCategoryAbbr LetterNumber         = "Nl"
generalCategoryAbbr OtherNumber          = "No"
generalCategoryAbbr ConnectorPunctuation = "Pc"
generalCategoryAbbr DashPunctuation      = "Pd"
generalCategoryAbbr OpenPunctuation      = "Ps"
generalCategoryAbbr ClosePunctuation     = "Pe"
generalCategoryAbbr InitialQuote         = "Pi"
generalCategoryAbbr FinalQuote           = "Pf"
generalCategoryAbbr OtherPunctuation     = "Po"
generalCategoryAbbr MathSymbol           = "Sm"
generalCategoryAbbr CurrencySymbol       = "Sc"
generalCategoryAbbr ModifierSymbol       = "Sk"
generalCategoryAbbr OtherSymbol          = "So"
generalCategoryAbbr Space                = "Zs"
generalCategoryAbbr LineSeparator        = "Zl"
generalCategoryAbbr ParagraphSeparator   = "Zp"
generalCategoryAbbr Control              = "Cc"
generalCategoryAbbr Format               = "Cf"
generalCategoryAbbr Surrogate            = "Cs"
generalCategoryAbbr PrivateUse           = "Co"
generalCategoryAbbr NotAssigned          = "Cn"

{-| The Unicode general category of the character.

This property is defined in the column 2 of the @UnicodeData@ table.

This relies on the 'Enum' instance of 'GeneralCategory', which must remain in the
same order as the categories are presented in the Unicode standard.

prop> show (generalCategory c) == show (Data.Char.generalCategory c)
-}
{-# INLINE generalCategory #-}
generalCategory :: Char -> GeneralCategory
generalCategory = toEnum . UC.generalCategory

{-| Selects alphabetic Unicode characters (lower-case, upper-case and title-case letters, plus letters of caseless scripts and modifiers letters).

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

* 'UppercaseLetter'
* 'LowercaseLetter'
* 'TitlecaseLetter'
* 'ModifierLetter'
* 'OtherLetter'

Alias of 'isLetter'.

__Note:__ this function is /not/ equivalent to 'isAlphabetic'.
See the description of 'isAlphabetic' for further details.

prop> isAlpha c == Data.Char.isAlpha c
-}
{-# INLINE isAlpha #-}
isAlpha :: Char -> Bool
isAlpha = isLetter

{-| Returns 'True' for alphabetic Unicode characters (lower-case, upper-case
and title-case letters, plus letters of caseless scripts and modifiers
letters).

__Note:__ this function is /not/ equivalent to 'isAlpha'/'isLetter':

* 'isAlpha' matches the following general categories:

    * 'UppercaseLetter' (@Lu@)
    * 'LowercaseLetter' (@Ll@)
    * 'TitlecaseLetter' (@Lt@)
    * 'ModifierLetter' (@Lm@)
    * 'OtherLetter' (@Lo@)

* whereas 'isAlphabetic' matches:

    * @Uppercase@ [property](https://www.unicode.org/reports/tr44/#Uppercase)
    * @Lowercase@ [property](https://www.unicode.org/reports/tr44/#Lowercase)
    * 'TitlecaseLetter' (@Lt@)
    * 'ModifierLetter' (@Lm@)
    * 'OtherLetter' (@Lo@)
    * 'LetterNumber' (@Nl@)
    * @Other_Alphabetic@ [property](https://www.unicode.org/reports/tr44/#Other_Alphabetic)
-}
{-# INLINE isAlphabetic #-}
isAlphabetic :: Char -> Bool
isAlphabetic = P.isAlphabetic

{-| Selects alphabetic or numeric Unicode characters.

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

* 'UppercaseLetter'
* 'LowercaseLetter'
* 'TitlecaseLetter'
* 'ModifierLetter'
* 'OtherLetter'
* 'DecimalNumber'
* 'LetterNumber'
* 'OtherNumber'

prop> isAlphaNum c == Data.Char.isAlphaNum c
-}
isAlphaNum :: Char -> Bool
isAlphaNum c = case generalCategory c of
  UppercaseLetter -> True
  LowercaseLetter -> True
  TitlecaseLetter -> True
  ModifierLetter  -> True
  OtherLetter     -> True
  DecimalNumber   -> True
  LetterNumber    -> True
  OtherNumber     -> True
  _               -> False

{-| Selects control characters, which are the non-printing characters
of the Latin-1 subset of Unicode.

This function returns 'True' if its argument has the 'GeneralCategory' 'Control'.

prop> isControl c == Data.Char.isControl c
-}
isControl :: Char -> Bool
isControl c = case generalCategory c of
  Control -> True
  _       -> False

{-| Selects alphabetic Unicode characters (lower-case, upper-case and title-case letters, plus letters of caseless scripts and modifiers letters).

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

* 'UppercaseLetter'
* 'LowercaseLetter'
* 'TitlecaseLetter'
* 'ModifierLetter'
* 'OtherLetter'

__Note:__ this function is /not/ equivalent to 'isAlphabetic'.
See the description of 'isAlphabetic' for further details.

prop> isLetter c == Data.Char.isLetter c
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

{-| Selects Unicode mark characters, for example accents and the
like, which combine with preceding characters.

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

* 'NonSpacingMark'
* 'SpacingCombiningMark'
* 'EnclosingMark'

prop> isMark c == Data.Char.isMark c
-}
isMark :: Char -> Bool
isMark c = case generalCategory c of
  NonSpacingMark       -> True
  SpacingCombiningMark -> True
  EnclosingMark        -> True
  _                    -> False

{-| Selects Unicode numeric characters, including digits from various
scripts, Roman numerals, et cetera.

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

* 'DecimalNumber'
* 'LetterNumber'
* 'OtherNumber'

prop> isNumber c == Data.Char.isNumber c
-}
isNumber :: Char -> Bool
isNumber c = case generalCategory c of
  DecimalNumber -> True
  LetterNumber  -> True
  OtherNumber   -> True
  _             -> False

{-| Selects printable Unicode characters (letters, numbers, marks, punctuation, symbols and spaces).

This function returns 'False' if its argument has one of the
following 'GeneralCategory's, or 'True' otherwise:

* 'LineSeparator'
* 'ParagraphSeparator'
* 'Control'
* 'Format'
* 'Surrogate'
* 'PrivateUse'
* 'NotAssigned'

prop> isPrint c == Data.Char.isPrint c
-}
isPrint :: Char -> Bool
isPrint c = case generalCategory c of
  LineSeparator      -> False
  ParagraphSeparator -> False
  Control            -> False
  Format             -> False
  Surrogate          -> False
  PrivateUse         -> False
  NotAssigned        -> False
  _                  -> True

{-| Selects Unicode punctuation characters, including various kinds
of connectors, brackets and quotes.

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

* 'ConnectorPunctuation'
* 'DashPunctuation'
* 'OpenPunctuation'
* 'ClosePunctuation'
* 'InitialQuote'
* 'FinalQuote'
* 'OtherPunctuation'

prop> isPunctuation c == Data.Char.isPunctuation c
-}
isPunctuation :: Char -> Bool
isPunctuation c = case generalCategory c of
  ConnectorPunctuation -> True
  DashPunctuation      -> True
  OpenPunctuation      -> True
  ClosePunctuation     -> True
  InitialQuote         -> True
  FinalQuote           -> True
  OtherPunctuation     -> True
  _                    -> False

{-| Selects Unicode space characters (general category 'Space'),
and the control characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.

__Note:__ 'isSpace' is /not/ equivalent to 'isWhiteSpace'.

prop> isSpace c == Data.Char.isSpace c
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

{- | Returns 'True' for any whitespace characters, and the control
characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.

See: [White_Space](https://www.unicode.org/reports/tr44/#White_Space).

__Note:__ 'isWhiteSpace' is /not/ equivalent to 'isSpace'.
-}
{-# INLINE isWhiteSpace #-}
isWhiteSpace :: Char -> Bool
isWhiteSpace = P.isWhite_Space

{-| Selects Unicode space and separator characters.

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

* 'Space'
* 'LineSeparator'
* 'ParagraphSeparator'

prop> isSeparator c == Data.Char.isSeparator c
-}
isSeparator :: Char -> Bool
isSeparator c = case generalCategory c of
  Space              -> True
  LineSeparator      -> True
  ParagraphSeparator -> True
  _                  -> False

{-| Selects Unicode symbol characters, including mathematical and currency symbols.

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:
* 'MathSymbol'
* 'CurrencySymbol'
* 'ModifierSymbol'
* 'OtherSymbol'

prop> isSymbol c == Data.Char.isSymbol c
-}
isSymbol :: Char -> Bool
isSymbol c = case generalCategory c of
  MathSymbol     -> True
  CurrencySymbol -> True
  ModifierSymbol -> True
  OtherSymbol    -> True
  _              -> False

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
