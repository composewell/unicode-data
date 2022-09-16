-- |
-- Module      : Main
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Main where

import GHC.Generics (Generic)
import Parser.Text (genCoreModules, genNamesModules, genScriptsModules, genSecurityModules)
import System.FilePath ((</>))
import WithCli (HasArguments(..), withCli)

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
    = genCoreModules (input opts </> "ucd") (output_core opts) (core_prop opts)
    *> genNamesModules (input opts </> "ucd") (output_names opts)
    *> genScriptsModules (input opts </> "ucd") (output_scripts opts)
    *> genSecurityModules (input opts </> "security") (output_security opts)

main :: IO ()
main = withCli cliClient
