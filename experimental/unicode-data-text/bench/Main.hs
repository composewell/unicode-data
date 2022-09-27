{-# LANGUAGE CPP #-}

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Test.Tasty.Bench
    (Benchmark, bgroup, bench, bcompare, defaultMain, env, nf)

import qualified Data.Text as T
import qualified Unicode.Char.General as G
import qualified Unicode.Text.Case as C

main :: IO ()
main = defaultMain
  [ bgroup "Unicode.Char.Case"
    [ bgroup "toLower"
      [ benchCaseConv "text" T.toLower
      , bcompare' "toLower" "text"
            (benchCaseConv "unicode-data (fusion)" C.toLowerFusion)
#if MIN_VERSION_text(2,0,0)
      , bcompare' "toLower" "text"
            (benchCaseConv "unicode-data (no fusion)" C.toLower)
#endif
      ]
    , bgroup "toUpper"
      [ benchCaseConv "text" T.toUpper
      , bcompare' "toUpper" "text"
            (benchCaseConv "unicode-data (fusion)" C.toUpperFusion)
#if MIN_VERSION_text(2,0,0)
      , bcompare' "toUpper" "text"
            (benchCaseConv "unicode-data (no fusion)" C.toUpper)
#endif
      ]
    , bgroup "toTitleText"
      [ benchCaseConv "text" T.toTitle
      , bcompare' "toTitleText" "text"
            (benchCaseConv "unicode-data (fusion)" C.toTitleFusion)
      ]
    , bgroup "toCaseFold"
      [ benchCaseConv "text" T.toCaseFold
      , bcompare' "toCaseFold" "text"
            (benchCaseConv "unicode-data (fusion)" C.toCaseFoldFusion)
#if MIN_VERSION_text(2,0,0)
      , bcompare' "toCaseFold" "text"
            (benchCaseConv "unicode-data (no fusion)" C.toCaseFold)
#endif
      ]
    ]
  ]
  where

    -- [NOTE] Works if groupTitle uniquely identifies the benchmark group.
    bcompare' :: String -> String -> Benchmark -> Benchmark
    bcompare' groupTitle ref = bcompare
        (mconcat ["$NF == \"", ref, "\" && $(NF-1) == \"", groupTitle, "\""])

    benchCaseConv
        :: String
        -> (T.Text -> T.Text)
        -> Benchmark
    benchCaseConv t f = benchNF t f (T.pack (filter isValid [minBound..maxBound]))
        -- where isValid c = G.generalCategory c < G.Surrogate
        where isValid = G.isAlphabetic

    benchNF
        :: forall a b. (NFData a, NFData b)
        => String
        -> (a -> b)
        -> a
        -> Benchmark
    benchNF t f a =
        -- Avoid side-effects with garbage collection (see tasty-bench doc)
        env
            (evaluate (force a)) -- initialize
            (bench t . nf f) -- benchmark
