module Unicode.Char.NumericBench
    ( benchmarks
    ) where

import Data.Int (Int64)
import Test.Tasty.Bench (Benchmark, bgroup)

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
  , bgroup "integerValue"
        [ bgroupWithChars "Integer" chars
            [ Bench "unicode-data" (Num.integerValue :: Char -> Maybe Integer)
            ]
        , bgroupWithChars "Int64" chars
            [ Bench "unicode-data" (Num.integerValue :: Char -> Maybe Int64)
            ]
        , bgroupWithChars "Int" chars
            [ Bench "unicode-data" (Num.integerValue :: Char -> Maybe Int)
            ]
        ]
  ]
