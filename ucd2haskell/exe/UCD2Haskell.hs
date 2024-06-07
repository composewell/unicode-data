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

data CLIOptions =
    CLIOptions
        { input :: FilePath
        , output_core :: FilePath
        -- ^ `unicode-data`
        , output_names :: FilePath
        -- ^ `unicode-data-names`
        , output_scripts :: FilePath
        -- ^ `unicode-data-scripts`
        , output_security :: FilePath
        -- ^ `unicode-data-security`
        , core_prop :: [String]
        }
    deriving (Show, Generic, HasArguments)

cliClient :: CLIOptions -> IO ()
cliClient opts
    = Core.generateModules (input opts </> "ucd") (output_core opts) (core_prop opts)
    *> Names.generateModules (input opts </> "ucd") (output_names opts)
    *> Scripts.generateModules (input opts </> "ucd") (output_scripts opts)
    *> Security.generateModules (input opts </> "security") (output_security opts)

main :: IO ()
main = withCli cliClient
