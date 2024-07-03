module Unicode.Char.General.CompatBench
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
import qualified Unicode.Char.General.Compat as GC

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks r = bgroupWithCharRange "Unicode.Char.General.Compat" r $ \chars ->
    [ bgroupWithChars "isAlpha" chars
      [ Bench "base"         Char.isAlpha
      , Bench "unicode-data" GC.isAlpha
      ]
    , bgroupWithChars "isAlphaNum" chars
      [ Bench "base"         Char.isAlphaNum
      , Bench "unicode-data" GC.isAlphaNum
      ]
    , bgroupWithChars "isLetter" chars
      [ Bench "base"         Char.isLetter
      , Bench "unicode-data" GC.isLetter
      ]
    , bgroupWithChars "isSpace" chars
      [ Bench "base"         Char.isSpace
      , Bench "unicode-data" GC.isSpace
      ]
    ]
