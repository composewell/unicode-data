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
import WithCli (HasArguments (..), Modifier (..), withCliModified)

import qualified UCD2Haskell.Generator.Core as Core
import qualified UCD2Haskell.Generator.Names as Names
import qualified UCD2Haskell.Generator.Scripts as Scripts
import qualified UCD2Haskell.Generator.Security as Security
import UCD2Haskell.Common (Version (..))
import UCD2Haskell.Generator (printCpuTime)

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
        , patterns :: [String]
        -- ^ Simple patterns to filter modules to generate.
        -- Leave empty to generate all modules.
        , unicode_version :: Version
        -- ^ Unicode version
        }
    deriving (Show, Generic, HasArguments)

cliClient :: CLIOptions -> IO ()
cliClient opts = do
    Core.generateModules version (inDir "ucd") (output_core opts) ps (core_prop opts)
    Names.generateModules version (inDir "ucd") (output_names opts) ps
    Scripts.generateModules version (inDir "ucd") (output_scripts opts) ps
    Security.generateModules version (inDir "security") (output_security opts) ps
    putChar '[' *> printCpuTime *> putStrLn "s] Finished"
  where
    version = unVersion (unicode_version opts)
    inDir = (input opts </>)
    ps = patterns opts

main :: IO ()
main = withCliModified
    [ AddShortOption "patterns" 'p']
    cliClient
