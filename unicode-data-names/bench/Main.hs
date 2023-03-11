{-# LANGUAGE CPP, ExistentialQuantification #-}

import Control.DeepSeq (NFData, deepseq)
import Data.Ix (Ix(..))
import Test.Tasty.Bench (Benchmark, bgroup, bcompare, bench, nf, defaultMain)

import qualified Unicode.Char.General.Names as Names
#ifdef HAS_ICU
import qualified ICU.Names as ICU
#endif

-- | A unit benchmark
data Bench = forall a. (NFData a) => Bench
  { -- | Name
    _title :: !String
    -- | Function to benchmark
  , _func :: Char -> a }

main :: IO ()
main = defaultMain
    [ bgroup "Unicode.Char.General.Names"
        [ bgroup' "name"
            [ Bench "unicode-data" Names.name
#ifdef HAS_ICU
            , Bench "icu"          ICU.name
#endif
            ]
        , bgroup' "correctedName"
            [ Bench "unicode-data" Names.correctedName
#ifdef HAS_ICU
            , Bench "icu"          ICU.correctedName
#endif
            ]
        , bgroup' "nameOrAlias"
            [ Bench "unicode-data" Names.name
            ]
        , bgroup' "nameAliasesByType"
            [ Bench "unicode-data"
                (\c -> (`Names.nameAliasesByType` c) <$> [minBound..maxBound])
            ]
        , bgroup' "nameAliasesWithTypes"
            [ Bench "unicode-data" (show . Names.nameAliasesWithTypes)
            ]
        , bgroup' "nameAliases"
            [ Bench "unicode-data" Names.nameAliases
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
        "unicode-data" -> benchNF title
        _              ->
            bcompare ( "$NF == \"unicode-data\" && $(NF-1) == \"" ++
                       groupTitle ++ "\"")
          . benchNF title

    benchNF :: forall a. (NFData a) => String -> (Char -> a) -> Benchmark
    benchNF t f = bench t $ nf (fold_ f) (minBound, maxBound)

    fold_ :: forall a. (NFData a) => (Char -> a) -> (Char, Char) -> ()
    fold_ f = foldr (deepseq . f) () . range
