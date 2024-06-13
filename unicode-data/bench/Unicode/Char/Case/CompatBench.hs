module Unicode.Char.Case.CompatBench
    ( benchmarks
    ) where

import qualified Data.Char as Char
import Test.Tasty.Bench (Benchmark)

import Unicode.Char.Bench (
    Bench (..),
    CharRange,
    bgroupWithCharRange,
    bgroupWithChars,
 )
import qualified Unicode.Char.Case.Compat as CC

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks r = bgroupWithCharRange "Unicode.Char.Case.Compat" r $ \chars ->
    [ bgroupWithChars "isLower" chars
      [ Bench "base"         Char.isLower
      , Bench "unicode-data" CC.isLower
      ]
    , bgroupWithChars "isUpper" chars
      [ Bench "base"         Char.isUpper
      , Bench "unicode-data" CC.isUpper
      ]
    , bgroupWithChars "toLower" chars
      [ Bench "base"         Char.toLower
      , Bench "unicode-data" CC.toLower
      ]
    , bgroupWithChars "toTitle" chars
      [ Bench "base"         Char.toTitle
      , Bench "unicode-data" CC.toTitle
      ]
    , bgroupWithChars "toUpper" chars
      [ Bench "base"         Char.toUpper
      , Bench "unicode-data" CC.toUpper
      ]
    ]
