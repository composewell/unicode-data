import Control.DeepSeq (NFData, deepseq, force)
import Control.Exception (evaluate)
import Data.Ix (Ix(..))
import Test.Tasty.Bench
    (Benchmark, bgroup, bench, defaultMain, env, nf)

#if MIN_VERSION_base(4,10,0) && !MIN_VERSION_base(4,15,0)
import qualified GHC.Magic as Exts (noinline)
#endif

import qualified Unicode.Char.General as G
import qualified Unicode.Char.General.Scripts as S

main :: IO ()
main = defaultMain
  [ bgroup "Unicode.Char.General.Script"
    [ bgroup "script"
      [ benchChars "unicode-data"  (show . S.script)
      ]
    , bgroup "scriptDefinition"
      [ benchNF "unicode-data"  (show . S.scriptDefinition)
      ]
    , bgroup "scriptExtensions"
      [ benchChars "unicode-data"  (show . S.scriptExtensions)
      ]
    ]
  ]
  where
    benchChars :: forall a. (NFData a) => String -> (Char -> a) -> Benchmark
    benchChars t f =
        -- Avoid side-effects with garbage collection (see tasty-bench doc)
        env
            (evaluate (force chars)) -- initialize
            (bench t . nf (foldString f)) -- benchmark
        where
        -- Filter out: Surrogates, Private Use Areas and unsassigned code points
        chars = filter isValid [minBound..maxBound]
        isValid c = G.generalCategory c < G.Surrogate

    foldString :: forall a. (NFData a) => (Char -> a) -> String -> ()
    foldString f = foldr (deepseq . f) ()

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
