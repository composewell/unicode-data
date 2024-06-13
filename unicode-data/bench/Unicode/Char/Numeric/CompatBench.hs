module Unicode.Char.Numeric.CompatBench
    ( benchmarks
    ) where

import Test.Tasty.Bench ( Benchmark )
import qualified Data.Char as Char

import Unicode.Char.Bench (
    Bench (..),
    CharRange,
    bgroupWithCharRange,
    bgroupWithChars,
 )
import qualified Unicode.Char.Numeric.Compat as NumCompat

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks r = bgroupWithCharRange "Unicode.Char.Numeric.Compat" r $ \chars ->
    [ bgroupWithChars "isNumber" chars
        [ Bench "base"         Char.isNumber
        , Bench "unicode-data" NumCompat.isNumber
        ]
    ]
