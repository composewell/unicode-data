-- |
-- Module      : Main
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Main where

import WithCli (HasArguments(..), withCli)
import Parser.Text (genCoreModules, genNamesModules)
import GHC.Generics (Generic)

data CLIOptions =
    CLIOptions
        { input :: FilePath
        , output_core :: FilePath
        -- ^ `unicode-data`
        , output_names :: FilePath
        -- ^ `unicode-data-names`
        , core_prop :: [String]
        }
    deriving (Show, Generic, HasArguments)

cliClient :: CLIOptions -> IO ()
cliClient opts
    = genCoreModules (input opts) (output_core opts) (core_prop opts)
    *> genNamesModules (input opts) (output_names opts)

main :: IO ()
main = withCli cliClient
