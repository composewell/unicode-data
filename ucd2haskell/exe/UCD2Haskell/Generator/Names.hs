-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Generator.Names
    ( generateModules
    ) where

import Data.Version (Version)
import System.FilePath ((</>))
import qualified Unicode.CharacterDatabase.Parser.Extracted.DerivedName as N
import qualified Unicode.CharacterDatabase.Parser.NameAliases as NA

import qualified UCD2Haskell.Modules.UnicodeData.DerivedNames as Names
import qualified UCD2Haskell.Modules.UnicodeData.NameAliases as NameAliases
import qualified UCD2Haskell.Modules.Version as Version
import UCD2Haskell.Generator (runGenerator)

generateModules :: Version -> FilePath -> FilePath -> IO ()
generateModules version indir outdir = do
    runGenerator
        version
        indir
        ("extracted" </> "DerivedName.txt")
        N.parse
        outdir
        [ Names.recipe ]

    runGenerator
        version
        indir
        "NameAliases.txt"
        NA.parse
        outdir
        [ NameAliases.recipe ]

    Version.writeModule
        version
        outdir
        "Unicode.Internal.Char.Names.Version"
        "0.3.0"
