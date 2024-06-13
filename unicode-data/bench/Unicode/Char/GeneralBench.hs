module Unicode.Char.GeneralBench
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
import qualified Unicode.Char.General as G

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks r = bgroupWithCharRange "Unicode.Char.General" r $ \chars ->
    -- Character classification
    [ bgroupWithChars "generalCategory" chars
      -- We use `fromEnum` because of incompatible GeneralCategory types
      [ Bench "base"         (fromEnum . Char.generalCategory)
      , Bench "unicode-data" (fromEnum . G.generalCategory)
      ]
    , bgroupWithChars "isAlphabetic" chars
      [ Bench "unicode-data" G.isAlphabetic
      ]
    , bgroupWithChars "isAlphaNum" chars
      [ Bench "base"         Char.isAlphaNum
      , Bench "unicode-data" G.isAlphaNum
      ]
    , bgroupWithChars "isControl" chars
      [ Bench "base"         Char.isControl
      , Bench "unicode-data" G.isControl
      ]
    , bgroupWithChars "isMark" chars
      [ Bench "base"         Char.isMark
      , Bench "unicode-data" G.isMark
      ]
    , bgroupWithChars "isPrint" chars
      [ Bench "base"         Char.isPrint
      , Bench "unicode-data" G.isPrint
      ]
    , bgroupWithChars "isPunctuation" chars
      [ Bench "base"         Char.isPunctuation
      , Bench "unicode-data" G.isPunctuation
      ]
    , bgroupWithChars "isSeparator" chars
      [ Bench "base"         Char.isSeparator
      , Bench "unicode-data" G.isSeparator
      ]
    , bgroupWithChars "isSymbol" chars
      [ Bench "base"         Char.isSymbol
      , Bench "unicode-data" G.isSymbol
      ]
    , bgroupWithChars "isWhiteSpace" chars
      [ Bench "unicode-data" G.isWhiteSpace
      ]
    -- Korean Hangul Characters
    , bgroupWithChars "isHangul" chars
      [ Bench "unicode-data" G.isHangul
      ]
    , bgroupWithChars "isHangulLV" chars
      [ Bench "unicode-data" G.isHangul
      ]
    , bgroupWithChars "isJamo" chars
      [ Bench "unicode-data" G.isJamo
      ]
    , bgroupWithChars "jamoLIndex" chars
      [ Bench "unicode-data" G.jamoLIndex
      ]
    , bgroupWithChars "jamoVIndex" chars
      [ Bench "unicode-data" G.jamoVIndex
      ]
    , bgroupWithChars "jamoTIndex" chars
      [ Bench "unicode-data" G.jamoTIndex
      ]
    ]
