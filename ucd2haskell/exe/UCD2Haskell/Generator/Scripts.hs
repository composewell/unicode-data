-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Generator.Scripts
    ( generateModules
    ) where

import qualified Data.ByteString as B
import Data.Version (Version)
import System.FilePath ((</>))
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop

import qualified UCD2Haskell.Modules.Scripts as Scripts
import qualified UCD2Haskell.Modules.ScriptsExtensions as ScriptsExtensions
import qualified UCD2Haskell.Modules.Version as Version
import UCD2Haskell.Generator (UnicodeSourceType(..), runGenerator)

generateModules :: Version -> FilePath -> FilePath -> [String] -> IO ()
generateModules version indir outdir patterns = do
    scriptAliases <- Scripts.parseScriptAliases
        <$> B.readFile (indir </> "PropertyValueAliases.txt")

    extensions <- ScriptsExtensions.parseScriptExtensions
        <$> B.readFile (indir </> "ScriptExtensions.txt")

    runGenerator
        version
        UCD
        indir
        "Scripts.txt"
        Prop.parse
        outdir
        patterns
        [ Scripts.recipe scriptAliases
        , ScriptsExtensions.recipe scriptAliases extensions ]

    Version.writeModule
        version
        outdir
        "Unicode.Internal.Char.Scripts.Version"
        "0.3.0"
