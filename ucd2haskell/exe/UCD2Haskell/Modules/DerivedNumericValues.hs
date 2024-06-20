-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.DerivedNumericValues (recipe) where

import qualified Data.ByteString.Builder as BB
import Data.Semigroup (Arg(..))
import qualified Data.Set as Set
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Extracted.DerivedNumericValues as N

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense)
import UCD2Haskell.Common (Fold (..), showB)

recipe :: FileRecipe N.Entry
recipe = ModuleRecipe
    "Unicode.Internal.Char.DerivedNumericValues"
    genNumericValuesModule

genNumericValuesModule :: BB.Builder -> Fold N.Entry BB.Builder
genNumericValuesModule moduleName = Fold step mempty done
    where

    step acc (N.Entry range value) = case range of
        U.SingleChar c -> Set.insert (Arg c value) acc
        U.CharRange{..} -> acc <> Set.fromDistinctAscList
            ((`Arg` value) <$> [start..end])

    mkNumericValue char value = mconcat
        [ "\n  "
        , showB char
        , " -> "
        , case value of
            N.Integer i -> "Just " <> BB.integerDec i
            N.Rational r -> showB (Just r)
        ]

    mkEntries = foldr
        (\(Arg c value) -> (<>) (mkNumericValue c value))
        mempty

    done values = unlinesBB
        [ apacheLicense 2022 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(numericValue)"
        , "where"
        , ""
        , "import Data.Ratio ((%))"
        , ""
        , "{-# NOINLINE numericValue #-}"
        , "numericValue :: Char -> Maybe Rational"
        , "numericValue = \\case" <> mkEntries values
        , "  _ -> Nothing"
        ]
