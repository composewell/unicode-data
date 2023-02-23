module Unicode.Char.GeneralBench
    ( benchmarks
    ) where

import Test.Tasty.Bench ( bgroup, Benchmark )

import Unicode.Char.Bench (benchChars, bgroup', Bench(..), CharRange)
import qualified Data.Char as Char
import qualified Unicode.Char.General as G

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks charRange = bgroup "Unicode.Char.General"
    -- Character classification
    [ bgroup' "generalCategory" charRange
      [ Bench "base"          (fromEnum . Char.generalCategory)
      , Bench "unicode-data"  (fromEnum . G.generalCategory)
      ]
    , bgroup "isAlphabetic"
      [ benchChars "unicode-data" charRange  G.isAlphabetic
      ]
    , bgroup' "isAlphaNum" charRange
      [ Bench "base"          Char.isAlphaNum
      , Bench "unicode-data"  G.isAlphaNum
      ]
    , bgroup' "isControl" charRange
      [ Bench "base"          Char.isControl
      , Bench "unicode-data"  G.isControl
      ]
    , bgroup' "isMark" charRange
      [ Bench "base"          Char.isMark
      , Bench "unicode-data"  G.isMark
      ]
    , bgroup' "isPrint" charRange
      [ Bench "base"          Char.isPrint
      , Bench "unicode-data"  G.isPrint
      ]
    , bgroup' "isPunctuation" charRange
      [ Bench "base"          Char.isPunctuation
      , Bench "unicode-data"  G.isPunctuation
      ]
    , bgroup' "isSeparator" charRange
      [ Bench "base"          Char.isSeparator
      , Bench "unicode-data"  G.isSeparator
      ]
    , bgroup' "isSymbol" charRange
      [ Bench "base"          Char.isSymbol
      , Bench "unicode-data"  G.isSymbol
      ]
    , bgroup "isWhiteSpace"
      [ benchChars "unicode-data" charRange  G.isWhiteSpace
      ]
    -- Korean Hangul Characters
    , bgroup "isHangul"
      [ benchChars "unicode-data" charRange  G.isHangul
      ]
    , bgroup "isHangulLV"
      [ benchChars "unicode-data" charRange  G.isHangul
      ]
    , bgroup "isJamo"
      [ benchChars "unicode-data" charRange  G.isJamo
      ]
    , bgroup "jamoLIndex"
      [ benchChars "unicode-data" charRange  G.jamoLIndex
      ]
    , bgroup "jamoVIndex"
      [ benchChars "unicode-data" charRange  G.jamoVIndex
      ]
    , bgroup "jamoTIndex"
      [ benchChars "unicode-data" charRange  G.jamoTIndex
      ]
    ]
