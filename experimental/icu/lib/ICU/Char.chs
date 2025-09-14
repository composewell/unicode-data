-- |
-- Module      : ICU.Char
-- Copyright   : (c) 2023 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode character general properties
--
-- @since 0.3.0

module ICU.Char
    ( unicodeVersion
    , charAge
    , UGeneralCategory(..)
    , toGeneralCategory
    , charType
    , UProperty(..)
    , hasBinaryProperty
    , isNoncharacter
    , isLowerCase
    , isUpperCase
    , isLower
    , isUpper
    , isTitle
    , toLowerCase
    , toUpperCase
    ) where

#include <unicode/uchar.h>

import Data.Char (chr, ord)
import qualified Data.Char as Char
import Data.Int (Int8)
import Data.Version (Version, makeVersion)
import Data.Word (Word32)
import Foreign (Ptr)
import Foreign.C (CInt(..))
import Foreign.Marshal.Array (allocaArray, peekArray)
import System.IO.Unsafe (unsafePerformIO)

type UChar32 = Word32

foreign import capi "icu.h value __hs_U_MAX_VERSION_LENGTH" maxVersionLength :: Int

foreign import ccall unsafe "icu.h __hs_u_getUnicodeVersion" u_getUnicodeVersion
    :: Ptr Int8 -> IO ()

-- | ICU Unicode version
unicodeVersion :: Version
unicodeVersion
    = makeVersion
    . fmap fromIntegral
    . unsafePerformIO
    $ allocaArray
        maxVersionLength
        (\ptr -> u_getUnicodeVersion ptr *> peekArray maxVersionLength ptr)

foreign import ccall unsafe "icu.h __hs_u_charAge" u_charAge
    :: UChar32 -> Ptr Int8 -> IO ()

-- | Character age
charAge :: Char -> Version
charAge c
    = makeVersion
    . fmap fromIntegral
    . unsafePerformIO
    $ allocaArray
        maxVersionLength
        (\ptr -> u_charAge cp ptr *> peekArray maxVersionLength ptr)
    where
    cp = fromIntegral (ord c)

foreign import ccall safe "icu.h __hs_u_charType" u_charType
    :: UChar32 -> Int8

{#enum define UGeneralCategory {
    U_UNASSIGNED as Unassigned,
    U_UPPERCASE_LETTER as UppercaseLetter,
    U_LOWERCASE_LETTER as LowercaseLetter,
    U_TITLECASE_LETTER as TitlecaseLetter,
    U_MODIFIER_LETTER as ModifierLetter,
    U_OTHER_LETTER as OtherLetter,
    U_NON_SPACING_MARK as NonSpacingMark,
    U_ENCLOSING_MARK as EnclosingMark,
    U_COMBINING_SPACING_MARK as CombiningSpacingMark,
    U_DECIMAL_DIGIT_NUMBER as DecimalDigitNumber,
    U_LETTER_NUMBER as LetterNumber,
    U_OTHER_NUMBER as OtherNumber,
    U_SPACE_SEPARATOR as SpaceSeparator,
    U_LINE_SEPARATOR as LineSeparator,
    U_PARAGRAPH_SEPARATOR as ParagraphSeparator,
    U_CONTROL_CHAR as ControlChar,
    U_FORMAT_CHAR as FormatChar,
    U_PRIVATE_USE_CHAR as PrivateUseChar,
    U_SURROGATE as Surrogate,
    U_DASH_PUNCTUATION as DashPunctuation,
    U_START_PUNCTUATION as StartPunctuation,
    U_END_PUNCTUATION as EndPunctuation,
    U_CONNECTOR_PUNCTUATION as ConnectorPunctuation,
    U_OTHER_PUNCTUATION as OtherPunctuation,
    U_MATH_SYMBOL as MathSymbol,
    U_CURRENCY_SYMBOL as CurrencySymbol,
    U_MODIFIER_SYMBOL as ModifierSymbol,
    U_OTHER_SYMBOL as OtherSymbol,
    U_INITIAL_PUNCTUATION as InitialPunctuation,
    U_FINAL_PUNCTUATION as FinalPunctuation
    }
    deriving (Bounded, Eq, Ord, Show) #}

-- | General category
charType :: Char -> UGeneralCategory
charType = toEnum . fromIntegral . u_charType . fromIntegral . ord

toGeneralCategory :: UGeneralCategory -> Char.GeneralCategory
toGeneralCategory = \case
    Unassigned -> Char.NotAssigned
    UppercaseLetter -> Char.UppercaseLetter
    LowercaseLetter -> Char.LowercaseLetter
    TitlecaseLetter -> Char.TitlecaseLetter
    ModifierLetter -> Char.ModifierLetter
    OtherLetter -> Char.OtherLetter
    NonSpacingMark -> Char.NonSpacingMark
    EnclosingMark -> Char.EnclosingMark
    CombiningSpacingMark -> Char.SpacingCombiningMark
    DecimalDigitNumber -> Char.DecimalNumber
    LetterNumber -> Char.LetterNumber
    OtherNumber -> Char.OtherNumber
    SpaceSeparator -> Char.Space
    LineSeparator -> Char.LineSeparator
    ParagraphSeparator -> Char.ParagraphSeparator
    ControlChar -> Char.Control
    FormatChar -> Char.Format
    PrivateUseChar -> Char.PrivateUse
    Surrogate -> Char.Surrogate
    DashPunctuation -> Char.DashPunctuation
    StartPunctuation -> Char.OpenPunctuation
    EndPunctuation -> Char.ClosePunctuation
    ConnectorPunctuation -> Char.ConnectorPunctuation
    OtherPunctuation -> Char.OtherPunctuation
    MathSymbol -> Char.MathSymbol
    CurrencySymbol -> Char.CurrencySymbol
    ModifierSymbol -> Char.ModifierSymbol
    OtherSymbol -> Char.OtherSymbol
    InitialPunctuation -> Char.InitialQuote
    FinalPunctuation -> Char.FinalQuote

{#enum define UProperty {
    UCHAR_NONCHARACTER_CODE_POINT as NoncharacterCodePoint,
    UCHAR_LOWERCASE as LowerCase,
    UCHAR_UPPERCASE as UpperCase
    }
    deriving (Bounded, Eq, Ord, Show) #}

foreign import ccall safe "icu.h __hs_u_hasBinaryProperty" u_hasBinaryProperty
    :: UChar32 -> CInt -> Bool

hasBinaryProperty :: Char -> UProperty -> Bool
hasBinaryProperty c
    = u_hasBinaryProperty (fromIntegral (ord c))
    . fromIntegral
    . fromEnum

isNoncharacter :: Char -> Bool
isNoncharacter = (`hasBinaryProperty` NoncharacterCodePoint)

isLowerCase :: Char -> Bool
isLowerCase = (`hasBinaryProperty` LowerCase)

isUpperCase :: Char -> Bool
isUpperCase = (`hasBinaryProperty` UpperCase)

foreign import ccall safe "icu.h __hs_u_islower" u_islower
    :: UChar32 -> Bool

isLower :: Char -> Bool
isLower = u_islower . fromIntegral . ord

foreign import ccall safe "icu.h __hs_u_isupper" u_isupper
    :: UChar32 -> Bool

isUpper :: Char -> Bool
isUpper = u_isupper . fromIntegral . ord

foreign import ccall safe "icu.h __hs_u_istitle" u_istitle
    :: UChar32 -> Bool

isTitle :: Char -> Bool
isTitle = u_istitle . fromIntegral . ord

foreign import ccall safe "icu.h __hs_u_tolower" u_tolower
    :: UChar32 -> UChar32

toLowerCase :: Char -> Char
toLowerCase = chr . fromIntegral . u_tolower . fromIntegral . ord

foreign import ccall safe "icu.h __hs_u_toupper" u_toupper
    :: UChar32 -> UChar32

toUpperCase :: Char -> Char
toUpperCase = chr . fromIntegral . u_toupper . fromIntegral . ord
