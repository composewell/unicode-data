module Unicode.Char.Case.CompatBench
    ( benchmarks
    ) where

import Test.Tasty.Bench ( bgroup, Benchmark )

import qualified Data.Char as Char
import Unicode.Char.Bench (CharRange, Bench(..), bgroup')
import qualified Unicode.Char.Case.Compat as CC

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks charRange = bgroup "Unicode.Char.Case.Compat"
    [ bgroup' "isLower" charRange
      [ Bench "base"         Char.isLower
      , Bench "unicode-data" CC.isLower
      ]
    , bgroup' "isUpper" charRange
      [ Bench "base"         Char.isUpper
      , Bench "unicode-data" CC.isUpper
      ]
    , bgroup' "toLower" charRange
      [ Bench "base"         Char.toLower
      , Bench "unicode-data" CC.toLower
      ]
    , bgroup' "toTitle" charRange
      [ Bench "base"         Char.toTitle
      , Bench "unicode-data" CC.toTitle
      ]
    , bgroup' "toUpper" charRange
      [ Bench "base"         Char.toUpper
      , Bench "unicode-data" CC.toUpper
      ]
    ]
