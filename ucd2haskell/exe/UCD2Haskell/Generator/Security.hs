-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Generator.Security
    ( generateModules
    ) where

import Data.Version (Version)
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop

import qualified UCD2Haskell.Modules.Security.Confusables as Confusables
import qualified UCD2Haskell.Modules.Security.IdentifierStatus as IdentifierStatus
import qualified UCD2Haskell.Modules.Security.IdentifierType as IdentifierType
import qualified UCD2Haskell.Modules.Security.IntentionalConfusables as IntentionalConfusables
import qualified UCD2Haskell.Modules.Version as Version
import UCD2Haskell.Generator (runGenerator)

generateModules :: Version -> FilePath -> FilePath -> [String] -> IO ()
generateModules version indir outdir patterns = do
    runGenerator
        version
        indir
        "IdentifierStatus.txt"
        Prop.parse
        outdir
        patterns
        [IdentifierStatus.recipe]

    runGenerator
        version
        indir
        "IdentifierType.txt"
        Prop.parse
        outdir
        patterns
        [IdentifierType.recipe]

    runGenerator
        version
        indir
        "confusables.txt"
        Prop.parseMultipleValues
        outdir
        patterns
        [Confusables.recipe]

    runGenerator
        version
        indir
        "intentional.txt"
        Prop.parse
        outdir
        patterns
        [IntentionalConfusables.recipe]

    Version.writeModule
        version
        outdir
        "Unicode.Internal.Char.Security.Version"
        "0.2.1"
