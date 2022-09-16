import Control.DeepSeq (NFData, deepseq, force)
import Control.Exception (evaluate)
import Test.Tasty.Bench
    (Benchmark, bgroup, bench, defaultMain, env, nf)

import qualified Unicode.Char.General as G
import qualified Unicode.Char.General.Scripts as S

main :: IO ()
main = defaultMain
  [ bgroup "Unicode.Char.General.Script"
    [ bgroup "script"
      [ benchNF "unicode-data"  (show . S.script)
      ]
    -- [TODO] scriptDefinition, inScript
    ]
  ]
  where
    benchNF :: forall a. (NFData a) => String -> (Char -> a) -> Benchmark
    benchNF t f =
        -- Avoid side-effects with garbage collection (see tasty-bench doc)
        env
            (evaluate (force chars)) -- initialize
            (bench t . nf (fold_ f)) -- benchmark
        where
        -- Filter out: Surrogates, Private Use Areas and unsassigned code points
        chars = filter isValid [minBound..maxBound]
        isValid c = G.generalCategory c < G.Surrogate

    fold_ :: forall a. (NFData a) => (Char -> a) -> String -> ()
    fold_ f = foldr (deepseq . f) ()
