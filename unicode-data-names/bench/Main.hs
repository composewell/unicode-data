{-# LANGUAGE ExistentialQuantification #-}

import Control.DeepSeq (NFData, deepseq)
import Data.Ix (Ix(..))
import Test.Tasty.Bench (Benchmark, bgroup, bcompare, bench, nf, defaultMain)

import qualified Unicode.Char.General.Names as Names

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
            [ Bench "String"  Names.name
            ]
        , bgroup' "correctedName"
            [ Bench "String"  Names.correctedName
            ]
        , bgroup' "nameOrAlias"
            [ Bench "String"  Names.name
            ]
        , bgroup' "nameAliasesByType"
            [ Bench "String"
                (\c -> (`Names.nameAliasesByType` c) <$> [minBound..maxBound])
            ]
        , bgroup' "nameAliasesWithTypes"
            [ Bench "String"  (show . Names.nameAliasesWithTypes)
            ]
        , bgroup' "nameAliases"
            [ Bench "String"  Names.nameAliases
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
