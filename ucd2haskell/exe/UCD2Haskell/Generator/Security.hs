-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Generator.Security
    ( generateModules
    ) where

import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop

import qualified UCD2Haskell.Modules.Security.Confusables as Confusables
import qualified UCD2Haskell.Modules.Security.IdentifierStatus as IdentifierStatus
import qualified UCD2Haskell.Modules.Security.IdentifierType as IdentifierType
import qualified UCD2Haskell.Modules.Security.IntentionalConfusables as IntentionalConfusables
import UCD2Haskell.Generator (runGenerator)

generateModules :: FilePath -> FilePath -> IO ()
generateModules indir outdir = do
    runGenerator
        indir
        "IdentifierStatus.txt"
        Prop.parse
        outdir
        [IdentifierStatus.recipe]

    runGenerator
        indir
        "IdentifierType.txt"
        Prop.parse
        outdir
        [IdentifierType.recipe]

    runGenerator
        indir
        "confusables.txt"
        Prop.parseMultipleValues
        outdir
        [Confusables.recipe]

    runGenerator
        indir
        "intentional.txt"
        Prop.parse
        outdir
        [IntentionalConfusables.recipe]
