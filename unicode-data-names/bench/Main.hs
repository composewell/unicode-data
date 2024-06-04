{-# LANGUAGE CPP, ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.DeepSeq (NFData, deepseq, force)
import Control.Exception (evaluate)
import Data.Ix (Ix(..))
import Data.Proxy (Proxy(..))
import GHC.Exts (Char(..), indexCharOffAddr#)
import Test.Tasty (askOption, includingOptions)
import Test.Tasty.Bench (Benchmark, bgroup, bcompare, bench, benchIngredients, nf, env)
import Test.Tasty.Options
    ( IsOption(defaultValue, optionHelp, optionName, parseValue)
    , OptionDescription(..) )
import Test.Tasty.Runners (TestTree, defaultMainWithIngredients)

import qualified Unicode.Char as UChar
import qualified Unicode.Char.General.Names as String
import qualified Unicode.Internal.Char.UnicodeData.DerivedName as DerivedName
import qualified Unicode.Internal.Char.UnicodeData.NameAliases as NameAliases
#ifdef HAS_ICU
import qualified ICU.Names as ICUString
#endif

--------------------------------------------------------------------------------
-- CLI options
--------------------------------------------------------------------------------

data CharRange = CharRange !Char !Char

instance IsOption CharRange where
    defaultValue = CharRange minBound maxBound
    parseValue = \case
        "ascii"      -> Just (CharRange minBound '\x7f')
        "bmp"        -> Just (CharRange minBound '\xffff')
        "planes0To3" -> Just (CharRange minBound '\x3FFFF')
        -- [TODO] handle errors
        s ->
          let (l, u) = drop 1 <$> break (== '-') s
          in Just (CharRange (UChar.chr (read l)) (UChar.chr (read u)))
    optionName = pure "chars"
    optionHelp = pure "Range of chars to test"

data Filter
    = NoFilter        -- ^ No condition
    | WithName        -- ^ Char has a name
    | WithNameAlias   -- ^ Char has a name alias
    | WithNameOrAlias -- ^ Char has a name or an alias

instance IsOption Filter where
    defaultValue = WithNameOrAlias
    parseValue = \case
        "name"        -> Just WithName
        "alias"       -> Just WithNameAlias
        "nameOrAlias" -> Just WithNameOrAlias
        "none"        -> Just NoFilter
        _             -> Nothing
    optionName = pure "chars-filter"
    optionHelp = pure "Filter the chars to test"

--------------------------------------------------------------------------------
-- Benchmark utils
--------------------------------------------------------------------------------

-- Orphan instance
instance NFData String.NameAliasType

-- | A unit benchmark
data Bench = forall a. (NFData a) => Bench
  { -- | Name
    _title :: !String
    -- | Function to benchmark
  , _func :: Char -> a }

hasName :: Char -> Bool
hasName (C# c#) = case DerivedName.name c# of
    (# _, 0# #) -> False
    _           -> True

hasNameAlias :: Char -> Bool
hasNameAlias (C# c#) =
    let addr# = NameAliases.nameAliases c#
    in case indexCharOffAddr# addr# 0# of
        '\xff'# -> False
        _       -> True

--------------------------------------------------------------------------------
-- Benchmark
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let customOpts  = [ Option (Proxy :: Proxy CharRange)
                    , Option (Proxy :: Proxy Filter)]
      ingredients = includingOptions customOpts : benchIngredients
  defaultMainWithIngredients ingredients
    (askOption (askOption . benchmarks))

benchmarks :: CharRange -> Filter -> TestTree
benchmarks charRange charFilter = bgroup "All"
    [ bgroup "Unicode.Char.General.Names"
        [ bgroup "name"
            [ bgroup' "name" "String"
                [ Bench "unicode-data" String.name
#ifdef HAS_ICU
                , Bench "icu"          ICUString.name
#endif
                ]
            ]
        , bgroup "correctedName"
            [ bgroup' "correctedName" "String"
                [ Bench "unicode-data" String.correctedName
#ifdef HAS_ICU
                , Bench "icu"          ICUString.correctedName
#endif
                ]
            ]
        , bgroup "nameOrAlias"
            [ bgroup' "nameOrAlias" "String"
                [ Bench "unicode-data" String.nameOrAlias
                ]
            ]
        , bgroup "nameAliasesByType"
            [ bgroup' "nameAliasesByType" "String"
                [ Bench "unicode-data"
                    (\c -> fold_ (`String.nameAliasesByType` c))
                ]
            ]
        , bgroup "nameAliasesWithTypes"
            [ bgroup' "nameAliasesWithTypes" "String"
                [ Bench "unicode-data" String.nameAliasesWithTypes
                ]
            ]
        , bgroup "nameAliases"
            [ bgroup' "nameAliases" "String"
                [ Bench "unicode-data" String.nameAliases
                ]
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
        "unicode-data" -> benchCharsNF title
        _              ->
            bcompare ( mconcat
                        [ "$NF == \"unicode-data\" && $(NF-1) == \""
                        , groupTitle
                        , "\" && $(NF-2) == \""
                        , superGroupTitle
                        , "\"" ] )
          . benchCharsNF title

    {-# INLINE benchCharsNF #-}
    benchCharsNF
        :: forall a. (NFData a)
        => String
        -> (Char -> a)
        -> Benchmark
    benchCharsNF t f =
        -- Avoid side-effects with garbage collection (see tasty-bench doc)
        env
            (evaluate (force chars'))               -- initialize
            (bench t . nf (foldr (deepseq . f) ())) -- benchmark
        where
        CharRange l u = charRange
        extraFilter = case charFilter of
            NoFilter -> const True
            WithName -> hasName
            WithNameAlias -> hasNameAlias
            WithNameOrAlias -> \c -> hasName c || hasNameAlias c
        chars = filter isValid [l..u]
        -- Ensure to have sufficiently chars
        n = 0x10FFFF `div` length chars
        chars' = mconcat (replicate n chars)
        isValid c = UChar.generalCategory c < UChar.Surrogate && extraFilter c

    fold_ :: forall a. (NFData a) => (String.NameAliasType -> a) -> ()
    fold_ f = foldr (deepseq . f) () (range (minBound, maxBound))
