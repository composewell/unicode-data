{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Unicode.Char.General.BlocksBench
    ( benchmarks
    ) where

import Control.DeepSeq (NFData (..))
import Test.Tasty.Bench (Benchmark, bgroup)

import Unicode.Char.Bench (
    Bench (..),
    CharRange (..),
    benchRange,
    bgroupWithCharRange',
 )
import qualified Unicode.Char.General.Blocks as B
import GHC.Generics (Generic)

-- FIXME derive Generic at datatype definition
deriving instance Generic B.BlockDefinition
instance NFData B.BlockDefinition

{-# NOINLINE benchmarks #-}
benchmarks :: CharRange -> Benchmark
benchmarks r = bgroup "Unicode.Char.General.Blocks"
    [ bgroupWithCharRange' "block" r
      [ Bench "unicode-data" (fmap fromEnum . B.block)
      ]
    , bgroup "blockDefinition"
      [ benchRange "unicode-data" B.blockDefinition
      ]
    ]
