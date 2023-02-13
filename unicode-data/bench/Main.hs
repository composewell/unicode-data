{-# LANGUAGE LambdaCase, CPP #-}

import Control.DeepSeq (NFData, deepseq, force)
import Control.Exception (evaluate)
import Data.Bifunctor (Bifunctor(..))
import Data.Ix (Ix(..))
import Data.Proxy
import Test.Tasty (askOption)
import Test.Tasty.Bench
    (Benchmark, bgroup, bench, bcompare, env, nf, benchIngredients)
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Options
import Test.Tasty.Runners

import qualified Data.Char as Char
import qualified Unicode.Char.Case as C
import qualified Unicode.Char.Case.Compat as CC
import qualified Unicode.Char.General as G
import qualified Unicode.Char.General.Blocks as B
import qualified Unicode.Char.General.Compat as GC
import qualified Unicode.Char.Identifiers as I
import qualified Unicode.Char.Normalization as N
import qualified Unicode.Char.Numeric as Num
import qualified Unicode.Char.Numeric.Compat as NumCompat

-- | A unit benchmark
data Bench a = Bench
  { _title :: !String  -- ^ Name
  , _func :: Char -> a -- ^ Function to benchmark
  }

data CharRange = CharRange !Char !Char

instance IsOption CharRange where
    defaultValue = CharRange minBound maxBound
    parseValue = \case
        "ascii"      -> Just (CharRange minBound '\x7f')
        "bmp"        -> Just (CharRange minBound '\xffff')
        "planes0To3" -> Just (CharRange minBound '\x3FFFF')
        -- [TODO] handle errors
        s -> Just (CharRange l u)
          where
            (l, u) = bimap (parseCodePoint minBound)
                           (parseCodePoint maxBound . drop 1)
                           (break (== '-') s)
            parseCodePoint def = \case
                "" -> def
                cp -> read ("0x" ++ cp)
    optionName = pure "chars"
    optionHelp = pure "Range of chars to test"

main :: IO ()
main = do
  let customOpts  = [Option (Proxy :: Proxy CharRange)]
      ingredients = includingOptions customOpts : benchIngredients
--   opts <- parseOptions ingredients (benchmarks charRange)
--   let charRange = lookupOption opts :: CharRange
--   defaultMainWithIngredients ingredients (benchmarks charRange)
  defaultMainWithIngredients ingredients (askOption benchmarks)

benchmarks :: CharRange -> TestTree
benchmarks charRange = bgroup "All"
  [ bgroup "Unicode.Char.Case"
    [
#if MIN_VERSION_base(4,18,0)
      bgroup' "isLowerCase"
      [ Bench "base"         Char.isLowerCase
      , Bench "unicode-data" C.isLowerCase
      ]
    , bgroup' "isUpperCase"
      [ Bench "base"         Char.isUpperCase
      , Bench "unicode-data" C.isUpperCase
      ]
#else
      bgroup "isLowerCase"
      [ benchChars "unicode-data" C.isLowerCase
      ]
    , bgroup "isUpperCase"
      [ benchChars "unicode-data" C.isUpperCase
      ]
#endif
    , bgroup "toCaseFoldString"
      [ benchChars "unicode-data" C.toCaseFoldString
      ]
    , bgroup "toLowerString"
      [ benchChars "unicode-data" C.toLowerString
      ]
    , bgroup "toTitleString"
      [ benchChars "unicode-data" C.toTitleString
      ]
    , bgroup "toUpperString"
      [ benchChars "unicode-data" C.toUpperString
      ]
    ]
  , bgroup "Unicode.Char.Case.Compat"
    [ bgroup' "isLower"
      [ Bench "base"         Char.isLower
      , Bench "unicode-data" CC.isLower
      ]
    , bgroup' "isUpper"
      [ Bench "base"         Char.isUpper
      , Bench "unicode-data" CC.isUpper
      ]
    , bgroup' "toLower"
      [ Bench "base"         Char.toLower
      , Bench "unicode-data" CC.toLower
      ]
    , bgroup' "toTitle"
      [ Bench "base"         Char.toTitle
      , Bench "unicode-data" CC.toTitle
      ]
    , bgroup' "toUpper"
      [ Bench "base"         Char.toUpper
      , Bench "unicode-data" CC.toUpper
      ]
    ]
  , bgroup "Unicode.Char.General"
    -- Character classification
    [ bgroup' "generalCategory"
      [ Bench "base"          (fromEnum . Char.generalCategory)
      , Bench "unicode-data"  (fromEnum . G.generalCategory)
      ]
    , bgroup "isAlphabetic"
      [ benchChars "unicode-data"  G.isAlphabetic
      ]
    , bgroup' "isAlphaNum"
      [ Bench "base"          Char.isAlphaNum
      , Bench "unicode-data"  G.isAlphaNum
      ]
    , bgroup' "isControl"
      [ Bench "base"          Char.isControl
      , Bench "unicode-data"  G.isControl
      ]
    , bgroup' "isMark"
      [ Bench "base"          Char.isMark
      , Bench "unicode-data"  G.isMark
      ]
    , bgroup' "isPrint"
      [ Bench "base"          Char.isPrint
      , Bench "unicode-data"  G.isPrint
      ]
    , bgroup' "isPunctuation"
      [ Bench "base"          Char.isPunctuation
      , Bench "unicode-data"  G.isPunctuation
      ]
    , bgroup' "isSeparator"
      [ Bench "base"          Char.isSeparator
      , Bench "unicode-data"  G.isSeparator
      ]
    , bgroup' "isSymbol"
      [ Bench "base"          Char.isSymbol
      , Bench "unicode-data"  G.isSymbol
      ]
    , bgroup "isWhiteSpace"
      [ benchChars "unicode-data"  G.isWhiteSpace
      ]
    -- Korean Hangul Characters
    , bgroup "isHangul"
      [ benchChars "unicode-data"  G.isHangul
      ]
    , bgroup "isHangulLV"
      [ benchChars "unicode-data"  G.isHangul
      ]
    , bgroup "isJamo"
      [ benchChars "unicode-data"  G.isJamo
      ]
    , bgroup "jamoLIndex"
      [ benchChars "unicode-data"  G.jamoLIndex
      ]
    , bgroup "jamoVIndex"
      [ benchChars "unicode-data"  G.jamoVIndex
      ]
    , bgroup "jamoTIndex"
      [ benchChars "unicode-data"  G.jamoTIndex
      ]
    ]
  , bgroup "Unicode.Char.General.Blocks"
    [ bgroup "block"
      [ benchChars "unicode-data"  (show . B.block)
      ]
    , bgroup "blockDefinition"
      [ benchRangeNF "unicode-data"  (show . B.blockDefinition)
      ]
    ]
  , bgroup "Unicode.Char.General.Compat"
    [ bgroup' "isAlpha"
      [ Bench "base"          Char.isAlpha
      , Bench "unicode-data"  GC.isAlpha
      ]
    , bgroup' "isLetter"
      [ Bench "base"          Char.isLetter
      , Bench "unicode-data"  GC.isLetter
      ]
    , bgroup' "isSpace"
      [ Bench "base"          Char.isSpace
      , Bench "unicode-data"  GC.isSpace
      ]
    ]
  , bgroup "Unicode.Char.Identifiers"
    [ bgroup "isIDContinue"
      [ benchChars "unicode-data"  I.isIDContinue
      ]
    , bgroup "isIDStart"
      [ benchChars "unicode-data"  I.isIDStart
      ]
    , bgroup "isXIDContinue"
      [ benchChars "unicode-data"  I.isXIDContinue
      ]
    , bgroup "isXIDStart"
      [ benchChars "unicode-data"  I.isXIDStart
      ]
    , bgroup "isPatternSyntax"
      [ benchChars "unicode-data"  I.isPatternSyntax
      ]
    , bgroup "isPatternWhitespace"
      [ benchChars "unicode-data"  I.isPatternWhitespace
      ]
    ]
  , bgroup "Unicode.Char.Normalization"
    [ bgroup "isCombining"
      [ benchChars "unicode-data"  N.isCombining
      ]
    , bgroup "combiningClass"
      [ benchChars "unicode-data"  N.combiningClass
      ]
    , bgroup "isCombiningStarter"
      [ benchChars "unicode-data"  N.isCombiningStarter
      ]
    -- [TODO] compose, composeStarters
    , bgroup "isDecomposable"
      [ bgroup "Canonical"
        [ benchChars "unicode-data" (N.isDecomposable N.Canonical)
        ]
      , bgroup "Kompat"
        [ benchChars "unicode-data" (N.isDecomposable N.Kompat)
        ]
      ]
    , bgroup "decompose"
      [ bgroup "Canonical"
        [ benchDecomposableChars "unicode-data" N.Canonical N.decompose
        ]
      , bgroup "Kompat"
        [ benchDecomposableChars "unicode-data" N.Kompat N.decompose
        ]
      ]
    , bgroup "decomposeHangul"
      [ benchChars "unicode-data" N.decomposeHangul
      ]
    ]
  , bgroup "Unicode.Char.Numeric"
    -- [TODO] Replace with 'isNumber' once the migration is done.
    [ bgroup "isNumeric"
      [ benchChars "unicode-data"  Num.isNumeric
      ]
    , bgroup "numericValue"
      [ benchChars "unicode-data" Num.numericValue
      ]
    , bgroup "integerValue"
      [ benchChars "unicode-data" Num.integerValue
      ]
    ]
  , bgroup "Unicode.Char.Numeric.Compat"
    [ bgroup' "isNumber"
      [ Bench "base"          Char.isNumber
      , Bench "unicode-data"  NumCompat.isNumber
      ]
    ]
  ]
  where
    bgroup' groupTitle bs = bgroup groupTitle
      [ benchChars' groupTitle title f
      | Bench title f <- bs
      ]

    -- [NOTE] Works if groupTitle uniquely identifies the benchmark group.
    bcompare' :: String -> String -> Benchmark -> Benchmark
    bcompare' groupTitle ref = bcompare
        (mconcat ["$NF == \"", ref, "\" && $(NF-1) == \"", groupTitle, "\""])

    benchChars' groupTitle title = case title of
      "base" -> benchChars title
      _      -> bcompare' groupTitle "base" . benchChars title

    benchChars :: forall a. (NFData a) => String -> (Char -> a) -> Benchmark
    benchChars t = benchCharsNF t isValid
        where
        -- Filter out: Surrogates, Private Use Areas and unsassigned code points
        isValid c = G.generalCategory c < G.Surrogate

    benchDecomposableChars
        :: forall a. (NFData a)
        => String
        -> N.DecomposeMode
        -> (N.DecomposeMode -> Char -> a)
        -> Benchmark
    benchDecomposableChars t mode f = benchCharsNF t isValid (f mode)
        where
        -- Filter out: Surrogates, Private Use Areas and unsassigned code points
        --             and non-decomposable characters
        isValid c = G.generalCategory c < G.Surrogate && N.isDecomposable mode c

    benchCharsNF
        :: forall a. (NFData a)
        => String
        -> (Char -> Bool)
        -> (Char -> a)
        -> Benchmark
    benchCharsNF t isValid f =
        -- Avoid side-effects with garbage collection (see tasty-bench doc)
        env
            (evaluate (force chars')) -- initialize
            (bench t . nf (foldString f)) -- benchmark
        where
        CharRange l u = charRange
        chars = filter isValid [l..u]
        -- Ensure to have sufficiently chars
        n = 0x10FFFF `div` length chars
        chars' = mconcat (replicate n chars)

    foldString :: forall a. (NFData a) => (Char -> a) -> String -> ()
    foldString f = foldr (deepseq . f) ()

    benchRangeNF
        :: forall a b. (Bounded a, Ix a, NFData b)
        => String
        -> (a -> b)
        -> Benchmark
    benchRangeNF t f = bench t (nf (fold_ f) (minBound, maxBound))

    fold_
        :: forall a b. (Ix a, NFData b)
        => (a -> b)
        -> (a, a)
        -> ()
    fold_ f = foldr (deepseq . f) () . range
