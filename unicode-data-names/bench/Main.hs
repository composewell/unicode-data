{-# LANGUAGE ExistentialQuantification #-}

import Control.DeepSeq (NFData, deepseq)
import Data.Ix (Ix(..))
import Test.Tasty.Bench (Benchmark, bgroup, bcompare, bench, nf, defaultMain)

import qualified Unicode.Char.General.Names as Names
import qualified Unicode.Internal.Char.UnicodeData.DerivedName as DerivedName
import qualified Unicode.Internal.Char.UnicodeData.NameAliases as NameAliases
import System.IO.Unsafe (unsafePerformIO)

-- | A unit benchmark
data Bench = forall a. (NFData a) => Bench
  { _title :: !String  -- ^ Name
  , _func :: Char -> a -- ^ Function to benchmark
  }

main :: IO ()
main = defaultMain
    [ bgroup "Unicode.Char.General.Names"
        -- Character classification
        [ bgroup' "name"
            [ Bench "CString" DerivedName.name
            , Bench "String"  Names.name
            ]
        , bgroup' "correctedName"
            [ Bench "String"  Names.correctedName
            ]
        , bgroup' "nameOrAlias"
            [ Bench "String"  Names.nameOrAlias
            ]
        , bgroup' "nameOrLabel"
            [ Bench "String"  Names.nameOrLabel
            ]
        , bgroup' "nameAliasesByType"
            [ Bench "CString"
                (\c -> (`NameAliases.nameAliasesByType` c) <$> [minBound..maxBound])
            , Bench "String"
                (\c -> (`Names.nameAliasesByType` c) <$> [minBound..maxBound])
            ]
        , bgroup' "nameAliasesWithTypes"
            [ Bench "CString" (show . NameAliases.nameAliasesWithTypes)
            , Bench "String"  (show . Names.nameAliasesWithTypes)
            ]
        , bgroup' "label"
            [ Bench "CString" (unsafePerformIO . DerivedName.label)
            , Bench "String"  Names.label
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
