{-# LANGUAGE CPP #-}

import Control.DeepSeq (NFData, deepseq, force)
import Control.Exception (evaluate)
import Data.Ix (Ix(..))
import Test.Tasty.Bench
    (Benchmark, bgroup, bench, defaultMain, env, nf)

import qualified Unicode.Char.General.Scripts as S
#ifdef HAS_ICU
import Data.Char ( ord )
import qualified ICU.Scripts as ICU
#endif

main :: IO ()
main = defaultMain
  [ bgroup "Unicode.Char.General.Script"
    [ bgroup "script"
      [ benchChars "unicode-data" id (fromEnum . S.script)
#ifdef HAS_ICU
      , benchChars "icu" (fromIntegral . ord) (fromEnum . ICU.codepointScript)
#endif
      ]
    , bgroup "scriptDefinition"
      [ benchNF "unicode-data" S.scriptDefinition
      ]
    , bgroup "scriptExtensions"
      [ benchChars "unicode-data" id (fmap fromEnum . S.scriptExtensions)
#ifdef HAS_ICU
      , benchChars "icu" id (fmap fromEnum . ICU.scriptExtensions)
#endif
      ]
    ]
  ]
  where
    benchChars
        :: forall a b. (NFData a, NFData b)
        => String
        -> (Char -> a)
        -> (a -> b)
        -> Benchmark
    benchChars t conv f =
        -- Avoid side-effects with garbage collection (see tasty-bench doc)
        env
            -- [NOTE] Replicate 10 times to ensure the function runs long enough
            (evaluate (force (mconcat (replicate 10 chars)))) -- initialize
            (bench t . nf (foldr (deepseq . f) ()))                     -- benchmark
        where
        -- Filter out: Surrogates, Private Use Areas and unsassigned code points
        chars = conv <$> filter isValid [minBound..maxBound]
        isValid c = S.script c /= S.Unknown

    benchNF
        :: forall a b. (Bounded a, Ix a, NFData b)
        => String
        -> (a -> b)
        -> Benchmark
    benchNF t f = bench t (nf (fold_ f) (minBound, maxBound))

    fold_
        :: forall a b. (Ix a, NFData b)
        => (a -> b)
        -> (a, a)
        -> ()
    fold_ f = foldr (deepseq . f) () . range
