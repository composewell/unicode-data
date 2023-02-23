{-# OPTIONS_GHC -Wno-orphans #-}

module Unicode.Char.General.BlocksBench
    ( benchmarks
    ) where

import Test.Tasty.Bench ( bgroup, Benchmark, bench, nf )

import Unicode.Char.Bench (benchChars, CharRange)
import qualified Unicode.Char.General.Blocks as B
import Control.DeepSeq (NFData, deepseq)
import Data.Ix (Ix(..))

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks charRange = bgroup "Unicode.Char.General.Blocks"
    [ bgroup "block"
      [ benchChars "unicode-data" charRange (fmap fromEnum . B.block)
      ]
    , bgroup "blockDefinition"
      [ benchRangeNF "unicode-data" B.blockDefinition
      ]
    ]

{-# INLINE benchRangeNF #-}
benchRangeNF
    :: forall a b. (Bounded a, Ix a, NFData b)
    => String
    -> (a -> b)
    -> Benchmark
benchRangeNF t f = bench t (nf (fold_ f) (minBound, maxBound))

{-# INLINE fold_ #-}
fold_
    :: forall a b. (Ix a, NFData b)
    => (a -> b)
    -> (a, a)
    -> ()
fold_ f = foldr (deepseq . f) () . range

-- NOTE: Orphan instance
instance NFData B.BlockDefinition
