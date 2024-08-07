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
    ( -- * Types of Code Points
      CodePointType(..)
    , codePointType

      -- * Unicode general categories
    , GeneralCategory(..)
    , generalCategoryAbbr
    , generalCategory

      -- * Character classification
    , isAlphabetic
    , isAlphaNum
    , isControl
    , isMark
    , isPrint
    , isPunctuation
    , isSeparator
    , isSymbol
    , isWhiteSpace
    , isNoncharacter

      -- ** Re-export
    , isAscii
    , isLatin1
    , isAsciiUpper
    , isAsciiLower

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
    , jamoLCount
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
import Data.Bits ((.&.))
import Data.Char (isAscii, isLatin1, isAsciiUpper, isAsciiLower, ord)
import Data.Ix (Ix)

import qualified Unicode.Char.General.Compat as Compat
import qualified Unicode.Internal.Char.DerivedCoreProperties as P
import qualified Unicode.Internal.Char.PropList as P
import qualified Unicode.Internal.Char.UnicodeData.GeneralCategory as UC
import Unicode.Internal.Division (quotRem28)

--------------------------------------------------------------------------------
-- General Category
--------------------------------------------------------------------------------

{-| Unicode General Categories.

These classes are defined in the
[Unicode Character Database](http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table),
part of the Unicode standard.

__Note:__ the classes must be in the same order they are listed in the Unicode Standard,
because some functions (e.g. 'generalCategory') rely on the 'Enum' instance.

@since 0.3.0
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
--
-- @since 0.3.0
generalCategoryAbbr :: GeneralCategory -> String
generalCategoryAbbr = \case
    UppercaseLetter      -> "Lu"
    LowercaseLetter      -> "Ll"
    TitlecaseLetter      -> "Lt"
    ModifierLetter       -> "Lm"
    OtherLetter          -> "Lo"
    NonSpacingMark       -> "Mn"
    SpacingCombiningMark -> "Mc"
    EnclosingMark        -> "Me"
    DecimalNumber        -> "Nd"
    LetterNumber         -> "Nl"
    OtherNumber          -> "No"
    ConnectorPunctuation -> "Pc"
    DashPunctuation      -> "Pd"
    OpenPunctuation      -> "Ps"
    ClosePunctuation     -> "Pe"
    InitialQuote         -> "Pi"
    FinalQuote           -> "Pf"
    OtherPunctuation     -> "Po"
    MathSymbol           -> "Sm"
    CurrencySymbol       -> "Sc"
    ModifierSymbol       -> "Sk"
    OtherSymbol          -> "So"
    Space                -> "Zs"
    LineSeparator        -> "Zl"
    ParagraphSeparator   -> "Zp"
    Control              -> "Cc"
    Format               -> "Cf"
    Surrogate            -> "Cs"
    PrivateUse           -> "Co"
    NotAssigned          -> "Cn"

{-| The Unicode general category of the character.

This property is defined in the column 2 of the @UnicodeData@ table.

This relies on the 'Enum' instance of 'GeneralCategory', which must remain in the
same order as the categories are presented in the Unicode standard.

prop> show (generalCategory c) == show (Data.Char.generalCategory c)

@since 0.3.0
-}
{-# INLINE generalCategory #-}
generalCategory :: Char -> GeneralCategory
generalCategory = toEnum . UC.generalCategory

--------------------------------------------------------------------------------
-- Types of Code Points
--------------------------------------------------------------------------------

-- | Types of Code Points.
--
-- These classes are defined in the section
-- [2.4 “Code Points and Characters”](https://www.unicode.org/versions/Unicode15.0.0/ch02.pdf#G14527)
-- of the Unicode standard.
--
-- @since 0.4.1
data CodePointType
    = GraphicType
    -- ^ __Graphic__: defined by the following general categories:
    --
    -- * Letters (L): 'UppercaseLetter', 'LowercaseLetter', 'TitlecaseLetter',
    --   'ModifierLetter', 'OtherLetter'.
    -- * Marks (M): 'NonSpacingMark', 'SpacingCombiningMark', 'EnclosingMark'.
    -- * Numbers (N): 'DecimalNumber', 'LetterNumber', 'OtherNumber'.
    -- * Punctuation (P): 'ConnectorPunctuation', 'DashPunctuation',
    --   'OpenPunctuation', 'ClosePunctuation', 'InitialQuote', 'FinalQuote',
    --   'OtherPunctuation'.
    -- * Symbol (S): 'MathSymbol', 'CurrencySymbol', 'ModifierSymbol',
    --   'OtherSymbol'.
    -- * Separators: 'Space'.
    | FormatType
    -- ^ __Format__: invisible but affects neighboring characters.
    --
    -- Defined by the following general categories:
    -- 'LineSeparator', 'ParagraphSeparator', 'Format'.
    | ControlType
    -- ^ __Control__: usage defined by protocols or standards outside the
    -- Unicode Standard.
    --
    -- Defined by the general category 'Control'.
    | PrivateUseType
    -- ^ __Private-use__: usage defined by private agreement outside the
    -- Unicode Standard.
    --
    -- Defined by the general category 'PrivateUse'.
    | SurrogateType
    -- ^ __Surrogate__: Permanently reserved for UTF-16.
    --
    -- Defined by the general category 'Surrogate'.
    | NoncharacterType
    -- ^ __Noncharacter:__ a code point that is permanently reserved for
    -- internal use (see definition D14 in the section
    -- [3.4 “Characters and Encoding”](https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2212)
    -- of the Unicode Standard).
    -- Noncharacters consist of the values @U+nFFFE@ and @U+nFFFF@ (where @n@
    -- is from 0 to 10₁₆) and the values @U+FDD0..U+FDEF@.
    --
    -- They are a subset of the general category 'NotAssigned'.
    | ReservedType
    -- ^ __Reserved:__ any code point of the Unicode Standard that is reserved
    -- for future assignment (see definition D15 in the section
    -- [3.4 “Characters and Encoding”](https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2212)
    -- of the Unicode Standard). Also known as an unassigned code point.
    --
    -- They are a subset of the general category 'NotAssigned'.
    deriving ( Show
             , Eq
             , Ord
             , Enum
             , Bounded
             , Ix
             )

-- | Returns the 'CodePointType' of a character.
--
-- @since 0.6.0
codePointType :: Char -> CodePointType
codePointType c = case generalCategory c of
    UppercaseLetter      -> GraphicType
    LowercaseLetter      -> GraphicType
    TitlecaseLetter      -> GraphicType
    ModifierLetter       -> GraphicType
    OtherLetter          -> GraphicType
    NonSpacingMark       -> GraphicType
    SpacingCombiningMark -> GraphicType
    EnclosingMark        -> GraphicType
    DecimalNumber        -> GraphicType
    LetterNumber         -> GraphicType
    OtherNumber          -> GraphicType
    ConnectorPunctuation -> GraphicType
    DashPunctuation      -> GraphicType
    OpenPunctuation      -> GraphicType
    ClosePunctuation     -> GraphicType
    InitialQuote         -> GraphicType
    FinalQuote           -> GraphicType
    OtherPunctuation     -> GraphicType
    MathSymbol           -> GraphicType
    CurrencySymbol       -> GraphicType
    ModifierSymbol       -> GraphicType
    OtherSymbol          -> GraphicType
    Space                -> GraphicType
    LineSeparator        -> FormatType
    ParagraphSeparator   -> FormatType
    Control              -> ControlType
    Format               -> FormatType
    Surrogate            -> SurrogateType
    PrivateUse           -> PrivateUseType
    NotAssigned
        | isNoncharacter c -> NoncharacterType
        | otherwise        -> ReservedType

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

{-| Returns 'True' for alphabetic Unicode characters (lower-case, upper-case
and title-case letters, plus letters of caseless scripts and modifiers
letters).

__Note:__ this function is /not/ equivalent to
'Unicode.Char.General.Compat.isAlpha' / 'Unicode.Char.General.Compat.isLetter':

* 'Unicode.Char.General.Compat.isAlpha' matches the following general categories:

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

@since 0.3.0
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

__Note:__ this function is incompatible with 'isAlphabetic':

>>> isAlphabetic '\x345'
True
>>> isAlphaNum '\x345'
False

@since 0.3.0
-}
{-# INLINE isAlphaNum #-}
{-# DEPRECATED isAlphaNum "Use Unicode.Char.General.Compat.isAlphaNum instead." #-}
isAlphaNum :: Char -> Bool
isAlphaNum = Compat.isAlphaNum

{-| Selects control characters, which are the non-printing characters
of the Latin-1 subset of Unicode.

This function returns 'True' if its argument has the 'GeneralCategory' 'Control'.

prop> isControl c == Data.Char.isControl c

@since 0.3.0
-}
isControl :: Char -> Bool
-- By definition (https://www.unicode.org/reports/tr44/#General_Category_Values)
-- “a C0 or C1 control code”, i.e. the 0x00-0x1f, 0x7f, and 0x80-0x9f.
isControl c = cp <= 0x9F && UC.generalCategoryPlanes0To3 cp == UC.Control
    where cp = ord c

{-| Selects Unicode mark characters, for example accents and the
like, which combine with preceding characters.

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

* 'NonSpacingMark'
* 'SpacingCombiningMark'
* 'EnclosingMark'

prop> isMark c == Data.Char.isMark c

@since 0.3.0
-}
isMark :: Char -> Bool
isMark c = UC.NonSpacingMark <= gc && gc <= UC.EnclosingMark
    where gc = UC.generalCategory c

{-| Selects printable Unicode characters (letters, numbers, marks, punctuation,
symbols and spaces).

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

@since 0.3.0
-}
isPrint :: Char -> Bool
isPrint c = UC.generalCategory c < UC.LineSeparator

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

@since 0.3.0
-}
isPunctuation :: Char -> Bool
isPunctuation c = UC.ConnectorPunctuation <= gc && gc <= UC.OtherPunctuation
    where gc = UC.generalCategory c

{- | Returns 'True' for any whitespace characters, and the control
characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.

See: [Unicode @White_Space@](https://www.unicode.org/reports/tr44/#White_Space).

__Note:__ 'isWhiteSpace' is /not/ equivalent to 'Unicode.Char.General.Compat.isSpace'.
'isWhiteSpace' selects the same characters from 'isSpace' plus the following:

* @U+0085@ NEXT LINE (NEL)
* @U+2028@ LINE SEPARATOR
* @U+2029@ PARAGRAPH SEPARATOR

@since 0.3.0
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

@since 0.3.0
-}
isSeparator :: Char -> Bool
isSeparator c =
    let !cp = ord c
    -- NOTE: The guard constant is updated at each Unicode revision.
    --       It must be < 0x40000 to be accepted by generalCategoryPlanes0To3.
    in cp <= UC.MaxIsSeparator &&
        let !gc = UC.generalCategoryPlanes0To3 cp
        in UC.Space <= gc && gc <= UC.ParagraphSeparator
    -- Use the following in case the previous code is not valid anymore:
    -- UC.Space <= gc && gc <= UC.ParagraphSeparator
    -- where gc = UC.generalCategory c

{-| Selects Unicode symbol characters, including mathematical and currency symbols.

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:
* 'MathSymbol'
* 'CurrencySymbol'
* 'ModifierSymbol'
* 'OtherSymbol'

prop> isSymbol c == Data.Char.isSymbol c

@since 0.3.0
-}
isSymbol :: Char -> Bool
isSymbol c = UC.MathSymbol <= gc && gc <= UC.OtherSymbol
    where gc = UC.generalCategory c

-- | Returns 'True' for any /noncharacter/.
--
-- A /noncharacter/ is a code point that is permanently reserved for internal
-- use (see definition D14 in the section
-- [3.4 “Characters and Encoding”](https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2212)
-- of the Unicode Standard).
--
-- Noncharacters consist of the values @U+nFFFE@ and @U+nFFFF@ (where @n@
-- is from 0 to 10₁₆) and the values @U+FDD0..U+FDEF@.
--
-- @since 0.6.0
isNoncharacter :: Char -> Bool
isNoncharacter c
    = ('\xFDD0' <= c && c <= '\xFDEF')
    || (ord c .&. 0xFFFF) >= 0xFFFE

-------------------------------------------------------------------------------
-- Korean Hangul
-------------------------------------------------------------------------------

-- jamo leading
jamoLFirst, jamoLCount, jamoLLast :: Int

-- | First leading consonant jamo.
--
-- @since 0.1.0
jamoLFirst  = 0x1100

-- | Total count of leading consonant jamo.
--
-- @since 0.3.0
jamoLCount = 19

-- | Last leading consonant jamo.
--
-- @since 0.1.0
jamoLLast = jamoLFirst + jamoLCount - 1

-- jamo vowel
jamoVFirst, jamoVCount, jamoVLast :: Int

-- | First vowel jamo.
--
-- @since 0.1.0
jamoVFirst  = 0x1161

-- | Total count of vowel jamo.
--
-- @since 0.1.0
jamoVCount = 21

-- | Last vowel jamo.
--
-- @since 0.1.0
jamoVLast = jamoVFirst + jamoVCount - 1

-- jamo trailing
jamoTFirst, jamoTCount :: Int

-- | The first trailing consonant jamo.
--
-- Note that 'jamoTFirst' does not represent a valid T, it represents a missing
-- T i.e. LV without a T. See comments under 'jamoTIndex' .
--
-- @since 0.1.0
jamoTFirst  = 0x11a7

-- | Total count of trailing consonant jamo.
--
-- @since 0.1.0
jamoTCount = 28

-- | Last trailing consonant jamo.
--
-- @since 0.1.0
jamoTLast :: Int
jamoTLast = jamoTFirst + jamoTCount - 1

-- | Total count of all jamo characters.
--
-- @jamoNCount = jamoVCount * jamoTCount@
--
-- @since 0.1.0
jamoNCount :: Int
jamoNCount = 588

-- Hangul
hangulFirst, hangulLast :: Int

-- | Codepoint of the first pre-composed Hangul character.
--
-- @since 0.1.0
hangulFirst = 0xac00

-- | Codepoint of the last Hangul character.
--
-- @since 0.1.0
hangulLast = hangulFirst + jamoLCount * jamoVCount * jamoTCount - 1

-- | Determine if the given character is a precomposed Hangul syllable.
--
-- @since 0.1.0
isHangul :: Char -> Bool
isHangul c = n >= hangulFirst && n <= hangulLast
    where n = ord c

-- | Determine if the given character is a Hangul LV syllable.
--
-- __Note:__ this function requires a precomposed Hangul syllable but does /not/
-- check it. Use 'isHangul' to check the input character before passing it to
-- 'isHangulLV'.
--
-- @since 0.1.0
isHangulLV :: Char -> Bool
isHangulLV c = assert (jamoTCount == 28)
    snd (quotRem28 (ord c - hangulFirst)) == 0

-- | Determine whether a character is a jamo L, V or T character.
--
-- @since 0.1.0
isJamo :: Char -> Bool
isJamo c = n >= jamoLFirst && n <= jamoTLast
    where n = ord c

-- | Given a Unicode character, if it is a leading jamo, return its index in
-- the list of leading jamo consonants, otherwise return 'Nothing'.
--
-- @since 0.1.0
jamoLIndex :: Char -> Maybe Int
jamoLIndex c
  | index >= 0 && index < jamoLCount = Just index
  | otherwise = Nothing
    where index = ord c - jamoLFirst

-- | Given a Unicode character, if it is a vowel jamo, return its index in the
-- list of vowel jamo, otherwise return 'Nothing'.
--
-- @since 0.1.0
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
-- @since 0.1.0
jamoTIndex :: Char -> Maybe Int
jamoTIndex c
  | index > 0 && index < jamoTCount = Just index
  | otherwise = Nothing
    where index = ord c - jamoTFirst
