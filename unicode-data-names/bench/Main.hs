{-# LANGUAGE CPP, ExistentialQuantification #-}

import Control.DeepSeq (NFData, deepseq)
import Data.Ix (Ix(..))
import Test.Tasty.Bench (Benchmark, bgroup, bcompare, bench, nf, defaultMain)

import qualified Unicode.Char.General.Names as String
#ifdef HAS_BYTESTRING
import qualified Unicode.Char.General.Names.ByteString as ByteString
import Data.ByteString ()
#endif
#ifdef HAS_TEXT
import qualified Unicode.Char.General.Names.Text as Text
import Data.Text ()
#endif
#ifdef HAS_ICU
import qualified ICU.Names as ICUString
#ifdef HAS_TEXT
import qualified ICU.Names.Text as ICUText
#endif
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
        [ bgroup "name"
            [ bgroup' "name" "String"
                [ Bench "unicode-data" String.name
#ifdef HAS_ICU
                , Bench "icu"          ICUString.name
#endif
            ]
#ifdef HAS_BYTESTRING
            , bgroup' "name" "ByteString"
                [ Bench "unicode-data" ByteString.name
-- #ifdef HAS_ICU
--                 , Bench "icu"          ICUByteString.name
-- #endif
                ]
#endif
#ifdef HAS_TEXT
            , bgroup' "name" "Text"
                [ Bench "unicode-data" Text.name
#ifdef HAS_ICU
                , Bench "icu"          ICUText.name
#endif
                ]
#endif
            ]
        , bgroup "correctedName"
            [ bgroup' "correctedName" "String"
                [ Bench "unicode-data" String.correctedName
#ifdef HAS_ICU
                , Bench "icu"          ICUString.correctedName
#endif
                ]
#ifdef HAS_BYTESTRING
            , bgroup' "name" "ByteString"
                [ Bench "unicode-data" ByteString.correctedName
-- #ifdef HAS_ICU
--                 , Bench "icu"          ICUByteString.correctedName
-- #endif
                ]
#endif
#ifdef HAS_TEXT
            , bgroup' "correctedName" "Text"
                [ Bench "unicode-data" Text.correctedName
#ifdef HAS_ICU
                , Bench "icu"          ICUText.correctedName
#endif
                ]
#endif
            ]
        , bgroup "nameOrAlias"
            [ bgroup' "nameOrAlias" "String"
                [ Bench "unicode-data" String.nameOrAlias
                ]
#ifdef HAS_BYTESTRING
            , bgroup' "nameOrAlias" "ByteString"
                [ Bench "unicode-data" ByteString.nameOrAlias
                ]
#endif
#ifdef HAS_TEXT
            , bgroup' "nameOrAlias" "Text"
                [ Bench "unicode-data" Text.nameOrAlias
                ]
#endif
            ]
        , bgroup "nameAliasesByType"
            [ bgroup' "nameAliasesByType" "String"
                [ Bench "unicode-data"
                    (\c -> (`String.nameAliasesByType` c) <$> [minBound..maxBound])
                ]
#ifdef HAS_BYTESTRING
            , bgroup' "nameAliasesByType" "ByteString"
                [ Bench "unicode-data"
                    (\c -> (`ByteString.nameAliasesByType` c) <$> [minBound..maxBound])
                ]
#endif
#ifdef HAS_TEXT
            , bgroup' "nameAliasesByType" "Text"
                [ Bench "unicode-data"
                    (\c -> (`Text.nameAliasesByType` c) <$> [minBound..maxBound])
                ]
#endif
            ]
        , bgroup "nameAliasesWithTypes"
            [ bgroup' "nameAliasesWithTypes" "String"
                [ Bench "unicode-data" (show . String.nameAliasesWithTypes)
                ]
#ifdef HAS_BYTESTRING
            , bgroup' "nameAliasesWithTypes" "ByteString"
                [ Bench "unicode-data" (show . ByteString.nameAliasesWithTypes)
                ]
#endif
#ifdef HAS_TEXT
            , bgroup' "nameAliasesWithTypes" "Text"
                [ Bench "unicode-data" (show . Text.nameAliasesWithTypes)
                ]
#endif
            ]
        , bgroup "nameAliases"
            [ bgroup' "nameAliases" "String"
                [ Bench "unicode-data" String.nameAliases
                ]
#ifdef HAS_BYTESTRING
            , bgroup' "nameAliases" "ByteString"
                [ Bench "unicode-data" ByteString.nameAliases
                ]
#endif
#ifdef HAS_TEXT
            , bgroup' "nameAliases" "Text"
                [ Bench "unicode-data" Text.nameAliases
                ]
#endif
            ]
        ]
    ]
  where
    bgroup' superGroupTitle groupTitle bs = bgroup groupTitle
        [ benchNF' superGroupTitle groupTitle title f
        | Bench title f <- bs
        ]

    -- [NOTE] Works if groupTitle uniquely identifies the benchmark group.
    benchNF' superGroupTitle groupTitle title = case title of
        "unicode-data" -> benchNF title
        _              ->
            bcompare ( mconcat
                        [ "$NF == \"unicode-data\" && $(NF-1) == \""
                        , groupTitle
                        , "\" && $(NF-2) == \""
                        , superGroupTitle
                        , "\"" ] )
          . benchNF title

    benchNF :: forall a. (NFData a) => String -> (Char -> a) -> Benchmark
    benchNF t f = bench t $ nf (fold_ f) (minBound, maxBound)

    fold_ :: forall a. (NFData a) => (Char -> a) -> (Char, Char) -> ()
    fold_ f = foldr (deepseq . f) () . range
