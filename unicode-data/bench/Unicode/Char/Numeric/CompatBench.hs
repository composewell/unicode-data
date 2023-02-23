module Unicode.Char.Numeric.CompatBench
    ( benchmarks
    ) where

import Test.Tasty.Bench ( bgroup, Benchmark )

import qualified Data.Char as Char
import Unicode.Char.Bench (Bench (..), CharRange, bgroup')
import qualified Unicode.Char.Numeric.Compat as NumCompat

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks charRange = bgroup "Unicode.Char.Numeric.Compat"
    [ bgroup' "isNumber" charRange
    [ Bench "base"          Char.isNumber
    , Bench "unicode-data"  NumCompat.isNumber
    ]
    ]
