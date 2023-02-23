module Unicode.Char.NormalizationBench
    ( benchmarks
    ) where

import Control.DeepSeq (NFData)
import Test.Tasty.Bench ( bgroup, Benchmark )

import Unicode.Char.Bench (benchChars, CharRange, benchCharsNF)
import qualified Unicode.Char.General as G
import qualified Unicode.Char.Normalization as N

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks charRange = bgroup "Unicode.Char.Normalization"
    [ bgroup "isCombining"
      [ benchChars "unicode-data" charRange N.isCombining
      ]
    , bgroup "combiningClass"
      [ benchChars "unicode-data" charRange  N.combiningClass
      ]
    , bgroup "isCombiningStarter"
      [ benchChars "unicode-data" charRange  N.isCombiningStarter
      ]
    -- [TODO] compose, composeStarters
    , bgroup "isDecomposable"
      [ bgroup "Canonical"
        [ benchChars "unicode-data" charRange (N.isDecomposable N.Canonical)
        ]
      , bgroup "Kompat"
        [ benchChars "unicode-data" charRange (N.isDecomposable N.Kompat)
        ]
      ]
    , bgroup "decompose"
      [ bgroup "Canonical"
        [ benchDecomposableChars "unicode-data" charRange N.Canonical N.decompose
        ]
      , bgroup "Kompat"
        [ benchDecomposableChars "unicode-data" charRange N.Kompat N.decompose
        ]
      ]
    , bgroup "decomposeHangul"
      [ benchChars "unicode-data" charRange N.decomposeHangul
      ]
    ]

{-# INLINE benchDecomposableChars #-}
benchDecomposableChars
    :: forall a. (NFData a)
    => String
    -> CharRange
    -> N.DecomposeMode
    -> (N.DecomposeMode -> Char -> a)
    -> Benchmark
benchDecomposableChars t charRange mode f =
    benchCharsNF t charRange isValid (f mode)
    where
    -- Filter out: Surrogates, Private Use Areas and unsassigned code points
    --             and non-decomposable characters
    isValid c = G.generalCategory c < G.Surrogate && N.isDecomposable mode c
