module Unicode.Char.NormalizationBench
    ( benchmarks
    ) where

import Test.Tasty.Bench ( bgroup, Benchmark )

import Unicode.Char.Bench (
    Bench (..),
    CharRange,
    bgroupWithCharRange,
    bgroupWithChars,
    bgroupWithValidCharRange',
 )
import qualified Unicode.Char.General as G
import qualified Unicode.Char.Normalization as N

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks r = bgroupWithCharRange "Unicode.Char.Normalization" r $ \chars ->
    [ bgroupWithChars "isCombining" chars
      [ Bench "unicode-data" N.isCombining
      ]
    , bgroupWithChars "combiningClass" chars
      [ Bench "unicode-data" N.combiningClass
      ]
    , bgroupWithChars "isCombiningStarter" chars
      [ Bench "unicode-data" N.isCombiningStarter
      ]
    -- [TODO] compose, composeStarters
    , bgroup "isDecomposable"
      [ bgroupWithChars "Canonical" chars
        [ Bench "unicode-data" (N.isDecomposable N.Canonical)
        ]
      , bgroupWithChars "Kompat" chars
        [ Bench "unicode-data" (N.isDecomposable N.Kompat)
        ]
      ]
    , bgroup "decompose"
      [ bgroupWithValidCharRange' "Canonical" r (isValid N.Canonical)
        [ Bench "unicode-data" (N.decompose N.Canonical)
        ]
      , bgroupWithValidCharRange' "Kompat" r (isValid N.Kompat)
        [ Bench "unicode-data" (N.decompose N.Kompat)
        ]
      ]
    , bgroupWithChars "decomposeHangul" chars
      [ Bench "unicode-data" N.decomposeHangul
      ]
    ]

-- Filter out: Surrogates, Private Use Areas and unsassigned code points
--             and non-decomposable characters
isValid :: N.DecomposeMode -> Char -> Bool
isValid mode c = G.generalCategory c < G.Surrogate && N.isDecomposable mode c
