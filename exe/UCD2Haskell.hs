{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

-- |
-- Module      : Script to parse Unicode XML database and convert
--               it to Haskell data structures
--
-- Copyright   : (c) 2014–2015 Antonio Nikishaev
--               (c) 2016-2017 Harendra Kumar
--
-- License     : BSD-3-Clause
-- Maintainer  : harendra.kumar@gmail.com
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
