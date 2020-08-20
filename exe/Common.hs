-- |
-- Module      : Common functions & data
--               (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental

module Common where

import Data.Bits (shiftL)
import Data.Char (chr)
import Data.List (unfoldr)

genSignature :: String -> String
genSignature testBit = testBit <> " :: Char -> Bool"

-- | Check that var is between minimum and maximum of orderList
genRangeCheck :: String -> [Int] -> String
genRangeCheck var ordList =
      var <> " >= "
      <> show (minimum ordList) <> " && " <> var <> " <= "
      <> show (maximum ordList)

genBitmap :: String -> [Int] -> String
genBitmap funcName ordList = unlines
  [ "{-# INLINE " ++ funcName  ++ " #-}"
  , genSignature funcName
  , funcName <> " = \\c -> let n = ord c in " ++ genRangeCheck "n" ordList ++ " && lookupBit64 bitmap# n"
  , "  where"
  , "    bitmap# = " ++ show (bitMapToAddrLiteral (positionsToBitMap ordList)) ++ "#"
  ]

positionsToBitMap :: [Int] -> [Bool]
positionsToBitMap = go 0
  where
    go _ [] = []
    go i xxs@(x : xs)
      | i < x     = False : go (i + 1) xxs
      | otherwise = True  : go (i + 1) xs

bitMapToAddrLiteral :: [Bool] -> String
bitMapToAddrLiteral = map (chr . toByte . padTo8) . unfoldr go
  where
    go :: [a] -> Maybe ([a], [a])
    go [] = Nothing
    go xs = Just (take 8 xs, drop 8 xs)

    padTo8 :: [Bool] -> [Bool]
    padTo8 xs
      | length xs >= 8 = xs
      | otherwise = xs ++ replicate (8 - length xs) False

    toByte :: [Bool] -> Int
    toByte xs = sum $ map (\i -> if xs !! i then 1 `shiftL` i else 0) [0..7]
