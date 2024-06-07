-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.UnicodeData.Composition
    ( recipe
    , parseFullCompositionExclusion) where

import qualified Data.ByteString.Builder as BB
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD
import qualified Unicode.CharacterDatabase.Parser.Properties.Multiple as Props

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense, genBitmap)
import UCD2Haskell.Common (Fold (..), showB, isHangulRange, allRange)
import Data.Char (ord)
import Data.Foldable (Foldable(..))
import qualified Data.Set as Set
import qualified Data.ByteString as B

recipe :: Set.Set Char -> Set.Set Char -> FileRecipe UD.Entry
recipe excluded combiningChars = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.Compositions"
    (\m -> genCompositionsModule m excluded combiningChars)

data Acc = Acc
    { decompositions :: ![BB.Builder]
    , starters :: ![BB.Builder]
    , secondStarters :: !(Set.Set Int) }

data Decomposition2 = Decomposition2
    { first :: !Char
    , second :: !Char }

genCompositionsModule
    :: BB.Builder
    -> Set.Set Char
    -> Set.Set Char
    -> Fold UD.Entry BB.Builder
genCompositionsModule moduleName excluded combiningChars =
    Fold step initial done

    where

    isNotExcluded = allRange (not . (`elem` excluded))

    genComposePairDef name c Decomposition2{..} = mconcat
        [ name
        , " "
        , showB first
        , " "
        , showB second
        , " = Just "
        , showB c ]

    initial = Acc mempty mempty mempty

    step acc (UD.Entry range (UD.decomposition -> decomp))
        | not (isHangulRange range) && isNotExcluded range =
            case decomp of
                UD.Decomposition UD.Canonical [c1, c2] ->
                        stepRange acc (Decomposition2 c1 c2) range
                _ -> acc
        -- Filtered out
        | otherwise = acc
    stepRange acc decomp = \case
        U.SingleChar c -> step' decomp acc c
        U.CharRange{..} -> foldl' (step' decomp) acc [start..end]
    step' decomp@Decomposition2{..} Acc{..} c = Acc
        { decompositions = decompositions'
        , starters= starters'
        , secondStarters = secondStarters' }

        where

        secondCP = ord second
        decompositions' = genComposePairDef "compose" c decomp : decompositions
        starters' =
            if second `notElem` combiningChars
            then genComposePairDef "composeStarters" c decomp : starters
            else starters
        secondStarters' =
            if second `notElem` combiningChars
            then Set.insert secondCP secondStarters
            else secondStarters

    header =
        [ apacheLicense 2020 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(compose, composeStarters, isSecondStarter)"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Data.Word (Word8)"
        , "import GHC.Exts (Ptr(..))"
        , "import Unicode.Internal.Bits (lookupBit64)"
        , ""
        ]

    composePair decomps =
        [ "{-# NOINLINE compose #-}"
        , "compose :: Char -> Char -> Maybe Char"
        , unlinesBB decomps
        , "compose _ _ = " <> "Nothing" <> "\n"
        , ""
        ]

    composeStarterPair starterPairs =
        [ "composeStarters :: Char -> Char -> Maybe Char"
        , unlinesBB starterPairs
        , "composeStarters _ _ = " <> "Nothing" <> "\n"
        , ""
        ]

    isSecondStarter secondStarters =
        [genBitmap "isSecondStarter" (Set.toAscList secondStarters)]

    done Acc{..} = unlinesBB . mconcat $
        [ header
        , composePair (reverse decompositions)
        , composeStarterPair (reverse starters)
        , isSecondStarter secondStarters ]

parseFullCompositionExclusion :: B.ByteString -> Set.Set Char
parseFullCompositionExclusion = foldr addExcluded mempty . Props.parse
    where
        addExcluded Props.Entry{..}
            | property /= "Full_Composition_Exclusion" = id
            | otherwise = case range of
                U.SingleChar c -> Set.insert c
                U.CharRange{..} -> (Set.fromList [start..end] <>)
