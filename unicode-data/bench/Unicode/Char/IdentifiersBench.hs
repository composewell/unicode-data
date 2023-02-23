module Unicode.Char.IdentifiersBench
    ( benchmarks
    ) where

import Test.Tasty.Bench ( bgroup, Benchmark )

import Unicode.Char.Bench (CharRange, benchChars)
import qualified Unicode.Char.Identifiers as I

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks charRange = bgroup "Unicode.Char.Identifiers"
    [ bgroup "isIDContinue"
      [ benchChars "unicode-data" charRange I.isIDContinue
      ]
    , bgroup "isIDStart"
      [ benchChars "unicode-data" charRange I.isIDStart
      ]
    , bgroup "isXIDContinue"
      [ benchChars "unicode-data" charRange I.isXIDContinue
      ]
    , bgroup "isXIDStart"
      [ benchChars "unicode-data" charRange I.isXIDStart
      ]
    , bgroup "isPatternSyntax"
      [ benchChars "unicode-data" charRange I.isPatternSyntax
      ]
    , bgroup "isPatternWhitespace"
      [ benchChars "unicode-data" charRange I.isPatternWhitespace
      ]
    ]
