-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Generator.Core
    ( generateModules
    ) where

import qualified Data.ByteString as B
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Version (Version)
import System.FilePath ((</>))
import qualified Unicode.CharacterDatabase.Parser.CaseFolding as CF
import qualified Unicode.CharacterDatabase.Parser.Extracted.DerivedNumericValues as N
import qualified Unicode.CharacterDatabase.Parser.Properties.Multiple as Props
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD

import qualified UCD2Haskell.Modules.Blocks as Blocks
import qualified UCD2Haskell.Modules.CaseFoldings as CaseFoldings
import qualified UCD2Haskell.Modules.DerivedNumericValues as DerivedNumericValues
import qualified UCD2Haskell.Modules.Properties as Properties
import qualified UCD2Haskell.Modules.SpecialCasings as SpecialCasings
import qualified UCD2Haskell.Modules.UnicodeData.CombiningClass as CombiningClass
import qualified UCD2Haskell.Modules.UnicodeData.Composition as Composition
import qualified UCD2Haskell.Modules.UnicodeData.Decomposition as Decomposition
import qualified UCD2Haskell.Modules.UnicodeData.GeneralCategory as GeneralCategory
import qualified UCD2Haskell.Modules.UnicodeData.SimpleCaseMappings as SimpleCaseMappings
import qualified UCD2Haskell.Modules.Version as Version
import UCD2Haskell.Generator (runGenerator)

generateModules :: Version -> FilePath -> FilePath -> [String] -> [String] -> IO ()
generateModules version indir outdir patterns props = do

    fullCompositionExclusion <- Composition.parseFullCompositionExclusion
        <$> B.readFile (indir </> "DerivedNormalizationProps.txt")

    combiningChars <- CombiningClass.parseCombining
        <$> B.readFile (indir </> "extracted" </> "DerivedCombiningClass.txt")

    specialCasings <- SpecialCasings.parse
        <$> B.readFile (indir </> "SpecialCasing.txt")

    let runGenerator' = runGenerator version indir

    runGenerator'
        "Blocks.txt"
        Prop.parse
        outdir
        patterns
        [ Blocks.recipe ]

    runGenerator'
        "UnicodeData.txt"
        UD.parse
        outdir
        patterns
        [ Composition.recipe fullCompositionExclusion combiningChars
        , CombiningClass.recipe
        , Decomposition.decomposable
        , Decomposition.decomposableK
        , Decomposition.decompositions
        , Decomposition.decompositionsK2
        , Decomposition.decompositionsK
        , GeneralCategory.recipe
        , SimpleCaseMappings.upperRecipe
        , SimpleCaseMappings.lowerRecipe
        , SimpleCaseMappings.titleRecipe
        , SpecialCasings.upperRecipe specialCasings
        , SpecialCasings.lowerRecipe specialCasings
        , SpecialCasings.titleRecipe specialCasings
        ]

    let propsSet = Set.fromList (fromString <$> props)

    runGenerator'
        "PropList.txt"
        Props.parse
        outdir
        patterns
        [ Properties.propList propsSet ]

    runGenerator'
        "DerivedCoreProperties.txt"
        Props.parse
        outdir
        patterns
        [ Properties.derivedCoreProperties propsSet ]

    runGenerator'
        "extracted/DerivedNumericValues.txt"
        N.parse
        outdir
        patterns
        [ DerivedNumericValues.recipe ]

    runGenerator'
        "CaseFolding.txt"
        CF.parse
        outdir
        patterns
        [ CaseFoldings.recipe ]

    Version.writeModule
        version
        outdir
        "Unicode.Internal.Char.Version"
        "0.3.0"
