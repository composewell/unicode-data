{-# LANGUAGE BlockArguments #-}

-------------------------------------------------------------------------------
-- |
-- Description: Export all characters and their properties to a CSV file.
--
-- This is intended to compare Haskell results to other languages.
-------------------------------------------------------------------------------

module Main where

import Data.Char
import Data.Foldable
import Data.Version
import Unicode.Char (unicodeVersion)
import Numeric

import qualified Unicode.Char.General.Names as UNames

main :: IO ()
main = do
  -- First line is Unicode version
  putStrLn (showVersion unicodeVersion)
  -- Second line is CSV header
  putStrLn header
  -- Then all the supported characters
  traverse_ addEntry [minBound..maxBound]

-- | File header
header :: String
header = "Char,Name"

-- | Convert a character to its (short) hexadecimal Unicode codepoint.
mkCodePointHex :: Char -> String
mkCodePointHex c = showHex (ord c) mempty

-- | Make a CSV entry for a char.
addEntry :: Char -> IO ()
addEntry c = do
  putStr (mkCodePointHex c)
  putChar ','
  putStrLn (show (UNames.name c))
