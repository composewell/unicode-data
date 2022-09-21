{-# LANGUAGE ExistentialQuantification #-}

import Control.DeepSeq (NFData, deepseq)
import Data.Ix (Ix(..))
import Test.Tasty.Bench (Benchmark, bgroup, bcompare, bench, nf, defaultMain)

import qualified Unicode.Char.Identifiers.Security as Security
import qualified Unicode.Internal.Char.Security.Confusables as Confusables
import qualified Unicode.Internal.Char.Security.IntentionalConfusables as Confusables

-- | A unit benchmark
data Bench = forall a. (NFData a) => Bench
  { _title :: !String  -- ^ Name
  , _func :: Char -> a -- ^ Function to benchmark
  }

main :: IO ()
main = defaultMain
    [ bgroup "Unicode.Char.Identifiers.Security"
        [ bgroup "Identifier Status"
            [ benchNF "isAllowedInIdentifier" (show . Security.isAllowedInIdentifier)
            ]
        , bgroup "Identifier Types"
            [ benchNF "identifierTypes"     (show . Security.identifierTypes)
            ]
        , bgroup "Confusables"
            [ bgroup' "prototypeIfConfusable"
                [ Bench "CString" Confusables.prototypeIfConfusable
                , Bench "String"  Security.prototypeIfConfusable
                ]
            , benchNF "prototype"                 Security.prototype
            , bgroup' "intentionalConfusables"
                [ Bench "CString" Confusables.intentionalConfusables
                , Bench "String"  Security.intentionalConfusables
                ]
            , benchNF "isIntentionallyConfusable" Security.intentionalConfusables
            ]
        ]
    ]
  where
    bgroup' groupTitle bs = bgroup groupTitle
        [ benchNF' groupTitle title f
        | Bench title f <- bs
        ]

    -- [NOTE] Works if groupTitle uniquely identifies the benchmark group.
    benchNF' groupTitle title = case title of
        "CString" -> benchNF title
        _         ->
            bcompare ("$NF == \"CString\" && $(NF-1) == \"" ++ groupTitle ++ "\"")
          . benchNF title

    benchNF :: forall a. (NFData a) => String -> (Char -> a) -> Benchmark
    benchNF t f = bench t $ nf (fold_ f) (minBound, maxBound)

    fold_ :: forall a. (NFData a) => (Char -> a) -> (Char, Char) -> ()
    fold_ f = foldr (deepseq . f) () . range
