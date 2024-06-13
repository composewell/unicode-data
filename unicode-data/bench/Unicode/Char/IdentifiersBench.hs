module Unicode.Char.IdentifiersBench
    ( benchmarks
    ) where

import Test.Tasty.Bench (Benchmark)

import Unicode.Char.Bench (
    Bench (..),
    CharRange,
    bgroupWithCharRange,
    bgroupWithChars,
 )
import qualified Unicode.Char.Identifiers as I

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks r = bgroupWithCharRange "Unicode.Char.Identifiers" r $ \chars ->
    [ bgroupWithChars "isIDContinue" chars
      [ Bench "unicode-data" I.isIDContinue
      ]
    , bgroupWithChars "isIDStart" chars
      [ Bench "unicode-data" I.isIDStart
      ]
    , bgroupWithChars "isXIDContinue" chars
      [ Bench "unicode-data" I.isXIDContinue
      ]
    , bgroupWithChars "isXIDStart" chars
      [ Bench "unicode-data" I.isXIDStart
      ]
    , bgroupWithChars "isPatternSyntax" chars
      [ Bench "unicode-data" I.isPatternSyntax
      ]
    , bgroupWithChars "isPatternWhitespace" chars
      [ Bench "unicode-data" I.isPatternWhitespace
      ]
    ]
