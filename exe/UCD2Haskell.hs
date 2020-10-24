-- |
-- Module      : Main
-- Description : Script to parse Unicode XML database and convert
--               it to Haskell data structures
--
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
--
module Main where

import WithCli (HasArguments(..), withCli)
import Parser.Text (genModules)
import GHC.Generics (Generic)

data CLIOptions =
    CLIOptions
        { input :: String
        , output :: String
        , core_prop :: [String]
        }
    deriving (Show, Generic, HasArguments)

cliClient :: CLIOptions -> IO ()
cliClient opts = genModules (input opts) (output opts) (core_prop opts)

main :: IO ()
main = withCli cliClient
