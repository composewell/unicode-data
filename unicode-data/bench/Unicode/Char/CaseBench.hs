{-# LANGUAGE CPP #-}

module Unicode.Char.CaseBench
    ( benchmarks
    ) where

import Test.Tasty.Bench ( bgroup, Benchmark )

import Unicode.Char.Bench (benchChars, CharRange)
import qualified Unicode.Char.Case as C

#if MIN_VERSION_base(4,18,0)
import qualified Data.Char as Char
import Unicode.Char.Bench (Bench(..), bgroup')
#endif

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks charRange = bgroup "Unicode.Char.Case"
    [
#if MIN_VERSION_base(4,18,0)
      bgroup' "isLowerCase" charRange
      [ Bench "base"         Char.isLowerCase
      , Bench "unicode-data" C.isLowerCase
      ]
    , bgroup' "isUpperCase" charRange
      [ Bench "base"         Char.isUpperCase
      , Bench "unicode-data" C.isUpperCase
      ]
#else
      bgroup "isLowerCase"
      [ benchChars "unicode-data" charRange C.isLowerCase
      ]
    , bgroup "isUpperCase"
      [ benchChars "unicode-data" charRange C.isUpperCase
      ]
#endif
    , bgroup "toCaseFoldString"
      [ benchChars "unicode-data" charRange C.toCaseFoldString
      ]
    , bgroup "toLowerString"
      [ benchChars "unicode-data" charRange C.toLowerString
      ]
    , bgroup "toTitleString"
      [ benchChars "unicode-data" charRange C.toTitleString
      ]
    , bgroup "toUpperString"
      [ benchChars "unicode-data" charRange C.toUpperString
      ]
    ]
