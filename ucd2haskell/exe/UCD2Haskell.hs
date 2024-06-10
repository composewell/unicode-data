-- |
-- Module      : Main
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Main where

import GHC.Generics (Generic)
import System.FilePath ((</>))
import WithCli (HasArguments(..), withCli)

import qualified UCD2Haskell.Generator.Core as Core
import qualified UCD2Haskell.Generator.Names as Names
import qualified UCD2Haskell.Generator.Scripts as Scripts
import qualified UCD2Haskell.Generator.Security as Security
import UCD2Haskell.Common (Version (..))

data CLIOptions =
    CLIOptions
        { input :: FilePath
        -- ^ Path to Unicode data files
        , output_core :: FilePath
        -- ^ Path to `unicode-data` lib directory
        , output_names :: FilePath
        -- ^ Path to `unicode-data-names` lib directory
        , output_scripts :: FilePath
        -- ^ Path to `unicode-data-scripts` lib directory
        , output_security :: FilePath
        -- ^ Path to `unicode-data-security` lib directory
        , core_prop :: [String]
        -- ^ Core properties to select
        , unicode_version :: Version
        -- ^ Unicode version
        }
    deriving (Show, Generic, HasArguments)

cliClient :: CLIOptions -> IO ()
cliClient opts
    = Core.generateModules version (inDir "ucd") (output_core opts) (core_prop opts)
    *> Names.generateModules version (inDir "ucd") (output_names opts)
    *> Scripts.generateModules version (inDir "ucd") (output_scripts opts)
    *> Security.generateModules version (inDir "security") (output_security opts)
    where
    version = unVersion (unicode_version opts)
    inDir = (input opts </>)

main :: IO ()
main = withCli cliClient
