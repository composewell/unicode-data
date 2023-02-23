module Unicode.Char.NumericBench
    ( benchmarks
    ) where

import Test.Tasty.Bench ( bgroup, Benchmark )

import Unicode.Char.Bench (benchChars, CharRange)
import qualified Unicode.Char.Numeric as Num

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks charRange = bgroup "Unicode.Char.Numeric"
  -- [TODO] Replace with 'isNumber' once the migration is done.
  [ bgroup "isNumeric"
    [ benchChars "unicode-data" charRange Num.isNumeric
    ]
  , bgroup "numericValue"
    [ benchChars "unicode-data" charRange Num.numericValue
    ]
  , bgroup "integerValue"
    [ benchChars "unicode-data" charRange Num.integerValue
    ]
  ]
