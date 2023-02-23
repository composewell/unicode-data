module Unicode.Char.General.CompatBench
    ( benchmarks
    ) where

import Test.Tasty.Bench ( bgroup, Benchmark )

import qualified Data.Char as Char
import Unicode.Char.Bench (CharRange, Bench(..), bgroup')
import qualified Unicode.Char.General.Compat as GC

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks charRange = bgroup "Unicode.Char.General.Compat"
    [ bgroup' "isAlpha" charRange
      [ Bench "base"          Char.isAlpha
      , Bench "unicode-data"  GC.isAlpha
      ]
    , bgroup' "isLetter" charRange
      [ Bench "base"          Char.isLetter
      , Bench "unicode-data"  GC.isLetter
      ]
    , bgroup' "isSpace" charRange
      [ Bench "base"          Char.isSpace
      , Bench "unicode-data"  GC.isSpace
      ]
    ]
