module Unicode.Char.NumericBench
    ( benchmarks
    ) where

import Test.Tasty.Bench (Benchmark)

import Unicode.Char.Bench (
    Bench (..),
    CharRange,
    bgroupWithCharRange,
    bgroupWithChars,
 )
import qualified Unicode.Char.Numeric as Num

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks r = bgroupWithCharRange "Unicode.Char.Numeric" r $ \chars ->
  -- [TODO] Replace with 'isNumber' once the migration is done.
  [ bgroupWithChars "isNumeric" chars
    [ Bench "unicode-data" Num.isNumeric
    ]
  , bgroupWithChars "numericValue" chars
    [ Bench "unicode-data" Num.numericValue
    ]
  , bgroupWithChars "integerValue" chars
    [ Bench "unicode-data" Num.integerValue
    ]
  ]
