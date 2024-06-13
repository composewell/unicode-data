{-# LANGUAGE CPP #-}

module Unicode.Char.CaseBench
    ( benchmarks
    ) where

import Test.Tasty.Bench (Benchmark)

import Unicode.Char.Bench (
    Bench (..),
    CharRange,
    bgroupWithCharRange,
    bgroupWithChars,
 )
import qualified Unicode.Char.Case as C

#if MIN_VERSION_base(4,18,0)
import qualified Data.Char as Char
#endif

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks r = bgroupWithCharRange "Unicode.Char.Case" r $ \chars ->
    [
#if MIN_VERSION_base(4,18,0)
      bgroupWithChars "isLowerCase" chars
      [ Bench "base"         Char.isLowerCase
      , Bench "unicode-data" C.isLowerCase
      ]
    , bgroupWithChars "isUpperCase" chars
      [ Bench "base"         Char.isUpperCase
      , Bench "unicode-data" C.isUpperCase
      ]
#else
      bgroupWithChars "isLowerCase" chars
      [ Bench "unicode-data" C.isLowerCase
      ]
    , bgroupWithChars "isUpperCase" chars
      [ Bench "unicode-data" C.isUpperCase
      ]
#endif
    , bgroupWithChars "toCaseFoldString" chars
      [ Bench "unicode-data" C.toCaseFoldString
      ]
    , bgroupWithChars "toLowerString" chars
      [ Bench "unicode-data" C.toLowerString
      ]
    , bgroupWithChars "toTitleString" chars
      [ Bench "unicode-data" C.toTitleString
      ]
    , bgroupWithChars "toUpperString" chars
      [ Bench "unicode-data" C.toUpperString
      ]
    ]
