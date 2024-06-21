-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.UnicodeData.GeneralCategory
    ( recipe
    ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
import Data.Foldable (Foldable (..))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD

import UCD2Haskell.Common (Fold (..), showB, showHexCodepointB)
import UCD2Haskell.Generator (
    FileRecipe (..),
    apacheLicense,
    genEnumBitmapShamochu,
    unlinesBB, ShamochuCode (..), mkImports,
 )
import Control.Exception (assert)

recipe :: FileRecipe UD.Entry
recipe = ModuleRecipe
         "Unicode.Internal.Char.UnicodeData.GeneralCategory"
         genGeneralCategoryModule

data GeneralCategoryAcc = GeneralCategoryAcc
  { _categories0_3 :: ![UD.GeneralCategory]
  , _categories14 :: ![UD.GeneralCategory]
  , _expectedChar :: !Char
  , _bounds :: !CharBounds
  }

genGeneralCategoryModule :: BB.Builder -> Fold UD.Entry BB.Builder
genGeneralCategoryModule moduleName = Fold step initial done

    where

    -- (categories planes 0-3, categories plane 14, expected char)
    initial = GeneralCategoryAcc
        []
        []
        minBound
        (CharBounds minBound minBound minBound minBound minBound minBound minBound)

    step :: GeneralCategoryAcc -> UD.Entry -> GeneralCategoryAcc
    step acc (UD.Entry range details) =
        case range of
            U.SingleChar c -> step' acc c details
            U.CharRange{..} -> foldl' (\a c -> step' a c details) acc [start..end]

    step' :: GeneralCategoryAcc -> Char -> UD.CharDetails -> GeneralCategoryAcc
    step' acc@(GeneralCategoryAcc acc1 acc2 p bounds) c details
        -- Plane 0 to 3, missing char
        -- Fill missing char entry with default category Cn
        -- See: https://www.unicode.org/reports/tr44/#Default_Values_Table
        | plane0To3 && p < c = step' (GeneralCategoryAcc (UD.Cn : acc1) acc2 (succ p) bounds)
            c details
        -- Plane 0 to 3, Regular entry
        | plane0To3 = GeneralCategoryAcc
            (generalCategory : acc1)
            acc2
            (succ c)
            (updateCharBounds bounds c generalCategory)
        -- Plane 4 to 13: no entry expected
        | plane4To13 = error ("Unexpected char in plane 4-13: " <> show (c, details))
        -- Plane 15 to 16: skip if PUA
        | plane15To16 = case generalCategory of
            UD.Co -> acc -- skip
            _  -> error ("Unexpected char in plane 15-16: " <> show (c, details))
        -- Leap to plane 14
        | p < '\xE0000' = step' (GeneralCategoryAcc acc1 acc2 '\xE0000' bounds) c details
        -- Plane 14, missing char
        | p < c = step' (GeneralCategoryAcc acc1 (UD.Cn : acc2) (succ p) bounds) c details
        -- Plane 14, regular entry
        | otherwise = GeneralCategoryAcc
            acc1
            (generalCategory : acc2)
            (succ c)
            (updateCharBounds bounds c generalCategory)
        where
        generalCategory = UD.generalCategory details
        plane0To3 = c <= '\x3FFFF'
        plane4To13 = c <= '\xDFFFF'
        plane15To16 = c >= '\xF0000'

    done (GeneralCategoryAcc acc1 acc2 _ CharBounds{..}) = unlinesBB
        [ apacheLicense 2020 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , "{-# LANGUAGE PatternSynonyms #-}"
        , ""
        , "module " <> moduleName
        , "( -- * Lookup functions"
        , "  generalCategory"
        , ", generalCategoryPlanes0To3"
        , ""
        , "  -- * General categories"
        , foldMapWithNewLine mkGeneralCategoryPatternExport [minBound..maxBound]
        , ""
        , "  -- * Characters bounds for predicates"
        , foldMapWithNewLine mkCharBoundPatternExport charBoundPatterns
        , ") where"
        , ""
        , mkImports imports
        , "--------------------------------------------------------------------------------"
        , "-- General category patterns"
        , "--------------------------------------------------------------------------------"
        , foldMap mkGeneralCategoryPattern [minBound..maxBound]
        , "--------------------------------------------------------------------------------"
        , "-- Characters bounds for predicates"
        , "--------------------------------------------------------------------------------"
        , foldMap mkCharBoundPattern charBoundPatterns
        , "--------------------------------------------------------------------------------"
        , "-- Lookup functions"
        , "--------------------------------------------------------------------------------"
        , ""
        , "-- | Return the general category of a code point in planes 0 to 3"
        , "--"
        , "-- The caller of this function must ensure its parameter is \\< @0x40000@."
        , "{-# INLINE generalCategoryPlanes0To3 #-}"
        , "generalCategoryPlanes0To3 :: Int -> Int"
        , "generalCategoryPlanes0To3 = lookupGeneralCategoryBitMap"
        , ""
        , "-- | Return the general category of a character"
        , code
        ]
        where
        toWord8 =
            assert (fromEnum (maxBound :: UD.GeneralCategory) < 0xff)
            (fromIntegral . fromEnum)
        foldMapWithNewLine f = mconcat . L.intersperse "\n" . fmap f
        mkExport p = ", pattern " <> p
        mkGeneralCategoryPatternExport = mkExport . generalCategoryConstructor
        mkGeneralCategoryPattern gc = mconcat
            [ "\n-- | General category ", showB gc, "\n"
            , "pattern ", generalCategoryConstructor gc, " :: Int\n"
            , "pattern ", generalCategoryConstructor gc
            , " = "
            , BB.intDec (fromEnum gc), "\n"]
        mkCharBoundPatternExport = mkExport . BB.shortByteString . fst
        mkCharBoundPattern :: (BS.ShortByteString, Char) -> BB.Builder
        mkCharBoundPattern (p, c) = mconcat
            [ "\n-- | Maximum codepoint satisfying @i", BB.shortByteString (BS.drop 4 p), "@\n"
            , "pattern ", BB.shortByteString p, " :: Int\n"
            , "pattern ", BB.shortByteString p, " = 0x", showHexCodepointB c, "\n"]
        charBoundPatterns :: [(BS.ShortByteString, Char)]
        charBoundPatterns =
            [ ("MaxIsLetter"   , maxIsLetter   )
            , ("MaxIsAlphaNum" , maxIsAlphaNum )
            , ("MaxIsLower"    , maxIsLower    )
            , ("MaxIsUpper"    , maxIsUpper    )
            , ("MaxIsNumber"   , maxIsNumber   )
            , ("MaxIsSpace"    , maxIsSpace    )
            , ("MaxIsSeparator", maxIsSeparator) ]
        ShamochuCode{..} = genEnumBitmapShamochu
            "generalCategory"
            False
            (NE.singleton 3)
            [5]
            toWord8
            (UD.Co, generalCategoryConstructor UD.Co)
            (UD.Cn, generalCategoryConstructor UD.Cn)
            (reverse acc1)
            (reverse acc2)

data CharBounds = CharBounds
    { maxIsLetter    :: !Char
    , maxIsAlphaNum  :: !Char
    , maxIsLower     :: !Char
    , maxIsUpper     :: !Char
    , maxIsNumber    :: !Char
    , maxIsSpace     :: !Char
    , maxIsSeparator :: !Char }

updateCharBounds :: CharBounds -> Char -> UD.GeneralCategory -> CharBounds
updateCharBounds acc@CharBounds{..} c = \case
    UD.Lu -> acc{ maxIsAlphaNum = max maxIsAlphaNum c
                , maxIsLetter = max maxIsLetter c
                , maxIsUpper = max maxIsUpper c }
    UD.Ll -> acc{ maxIsAlphaNum = max maxIsAlphaNum c
                , maxIsLetter = max maxIsLetter c
                , maxIsLower = max maxIsLower c }
    UD.Lt -> acc{ maxIsAlphaNum = max maxIsAlphaNum c
                , maxIsLetter = max maxIsLetter c
                , maxIsUpper = max maxIsUpper c }
    UD.Lm -> acc{maxIsAlphaNum=max maxIsAlphaNum c, maxIsLetter=max maxIsLetter c}
    UD.Lo -> acc{maxIsAlphaNum=max maxIsAlphaNum c, maxIsLetter=max maxIsLetter c}
    UD.Nd -> acc{maxIsAlphaNum=max maxIsAlphaNum c, maxIsNumber=max maxIsNumber c}
    UD.Nl -> acc{maxIsAlphaNum=max maxIsAlphaNum c, maxIsNumber=max maxIsNumber c}
    UD.No -> acc{maxIsAlphaNum=max maxIsAlphaNum c, maxIsNumber=max maxIsNumber c}
    UD.Zs -> acc{maxIsSeparator=max maxIsAlphaNum c, maxIsSpace=max maxIsSpace c}
    UD.Zl -> acc{maxIsSeparator=max maxIsAlphaNum c}
    UD.Zp -> acc{maxIsSeparator=max maxIsAlphaNum c}
    _  -> acc

generalCategoryConstructor :: UD.GeneralCategory -> BB.Builder
generalCategoryConstructor = \case
    UD.Lu -> "UppercaseLetter"
    UD.Ll -> "LowercaseLetter"
    UD.Lt -> "TitlecaseLetter"
    UD.Lm -> "ModifierLetter"
    UD.Lo -> "OtherLetter"
    UD.Mn -> "NonSpacingMark"
    UD.Mc -> "SpacingCombiningMark"
    UD.Me -> "EnclosingMark"
    UD.Nd -> "DecimalNumber"
    UD.Nl -> "LetterNumber"
    UD.No -> "OtherNumber"
    UD.Pc -> "ConnectorPunctuation"
    UD.Pd -> "DashPunctuation"
    UD.Ps -> "OpenPunctuation"
    UD.Pe -> "ClosePunctuation"
    UD.Pi -> "InitialQuote"
    UD.Pf -> "FinalQuote"
    UD.Po -> "OtherPunctuation"
    UD.Sm -> "MathSymbol"
    UD.Sc -> "CurrencySymbol"
    UD.Sk -> "ModifierSymbol"
    UD.So -> "OtherSymbol"
    UD.Zs -> "Space"
    UD.Zl -> "LineSeparator"
    UD.Zp -> "ParagraphSeparator"
    UD.Cc -> "Control"
    UD.Cf -> "Format"
    UD.Cs -> "Surrogate"
    UD.Co -> "PrivateUse"
    UD.Cn -> "NotAssigned"
