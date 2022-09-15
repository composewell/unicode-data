import Control.DeepSeq (NFData, deepseq, force)
import Control.Exception (evaluate)
import Test.Tasty.Bench
    (Benchmark, bgroup, bench, bcompare, defaultMain, env, nf)

import qualified Data.Char as Char
import qualified Unicode.Char.Case as C
import qualified Unicode.Char.Case.Compat as CC
import qualified Unicode.Char.General as G
import qualified Unicode.Char.General.Blocks as B
import qualified Unicode.Char.General.Compat as GC
import qualified Unicode.Char.General.Scripts as S
import qualified Unicode.Char.Identifiers as I
import qualified Unicode.Char.Normalization as N
import qualified Unicode.Char.Numeric as Num
import qualified Unicode.Char.Numeric.Compat as NumCompat

-- | A unit benchmark
data Bench a = Bench
  { _title :: !String  -- ^ Name
  , _func :: Char -> a -- ^ Function to benchmark
  }

main :: IO ()
main = defaultMain
  [ bgroup "Unicode.Char.Case"
    [ bgroup "isLowerCase"
      [ benchNF "unicode-data" C.isLowerCase
      ]
    , bgroup "isUpperCase"
      [ benchNF "unicode-data" C.isUpperCase
      ]
    , bgroup "toCaseFoldString"
      [ benchNF "unicode-data" C.toCaseFoldString
      ]
    , bgroup "toLowerString"
      [ benchNF "unicode-data" C.toLowerString
      ]
    , bgroup "toTitleString"
      [ benchNF "unicode-data" C.toTitleString
      ]
    , bgroup "toUpperString"
      [ benchNF "unicode-data" C.toUpperString
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
      [ Bench "base"          (show . Char.generalCategory)
      , Bench "unicode-data"  (show . G.generalCategory)
      ]
    , bgroup "isAlphabetic"
      [ benchNF "unicode-data"  G.isAlphabetic
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
      [ benchNF "unicode-data"  G.isWhiteSpace
      ]
    -- Korean Hangul Characters
    , bgroup "isHangul"
      [ benchNF "unicode-data"  G.isHangul
      ]
    , bgroup "isHangulLV"
      [ benchNF "unicode-data"  G.isHangul
      ]
    , bgroup "isJamo"
      [ benchNF "unicode-data"  G.isJamo
      ]
    , bgroup "jamoLIndex"
      [ benchNF "unicode-data"  G.jamoLIndex
      ]
    , bgroup "jamoVIndex"
      [ benchNF "unicode-data"  G.jamoVIndex
      ]
    , bgroup "jamoTIndex"
      [ benchNF "unicode-data"  G.jamoTIndex
      ]
    ]
  , bgroup "Unicode.Char.General.Blocks"
    [ bgroup "block"
      [ benchNF "unicode-data"  (show . B.block)
      ]
    -- [TODO] blockDefinition, inBlock
    , bgroup "allBlockRanges"
      [ benchNF "unicode-data"  (const B.allBlockRanges)
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
  , bgroup "Unicode.Char.General.Script"
    [ bgroup "script"
      [ benchNF "unicode-data"  (show . S.script)
      ]
    -- [TODO] scriptDefinition, inScript
    ]
  , bgroup "Unicode.Char.Identifiers"
    [ bgroup "isIDContinue"
      [ benchNF "unicode-data"  I.isIDContinue
      ]
    , bgroup "isIDStart"
      [ benchNF "unicode-data"  I.isIDStart
      ]
    , bgroup "isXIDContinue"
      [ benchNF "unicode-data"  I.isXIDContinue
      ]
    , bgroup "isXIDStart"
      [ benchNF "unicode-data"  I.isXIDStart
      ]
    , bgroup "isPatternSyntax"
      [ benchNF "unicode-data"  I.isPatternSyntax
      ]
    , bgroup "isPatternWhitespace"
      [ benchNF "unicode-data"  I.isPatternWhitespace
      ]
    ]
  , bgroup "Unicode.Char.Normalization"
    [ bgroup "isCombining"
      [ benchNF "unicode-data"  N.isCombining
      ]
    , bgroup "combiningClass"
      [ benchNF "unicode-data"  N.combiningClass
      ]
    , bgroup "isCombiningStarter"
      [ benchNF "unicode-data"  N.isCombiningStarter
      ]
    -- [TODO] compose, composeStarters
    , bgroup "isDecomposable"
      [ bgroup "Canonical"
        [ benchNF "unicode-data" (N.isDecomposable N.Canonical)
        ]
      , bgroup "Kompat"
        [ benchNF "unicode-data" (N.isDecomposable N.Kompat)
        ]
      ]
    -- [FIXME] Fail due to non-exhaustive pattern matching
    -- , bgroup "decompose"
    --   [ bgroup "Canonical"
    --     [ benchNF "unicode-data" (N.decompose N.Canonical)
    --     ]
    --   , bgroup "Kompat"
    --     [ benchNF "unicode-data" (N.decompose N.Kompat)
    --     ]
    --   ]
    , bgroup "decomposeHangul"
      [ benchNF "unicode-data" N.decomposeHangul
      ]
    ]
  , bgroup "Unicode.Char.Numeric"
    -- [TODO] Replace with 'isNumber' once the migration is done.
    [ bgroup "isNumeric"
      [ benchNF "unicode-data"  Num.isNumeric
      ]
    , bgroup "numericValue"
      [ benchNF "unicode-data" Num.numericValue
      ]
    , bgroup "integerValue"
      [ benchNF "unicode-data" Num.integerValue
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
      [ benchNF' groupTitle title f
      | Bench title f <- bs
      ]

    -- [NOTE] Works if groupTitle uniquely identifies the benchmark group.
    benchNF' groupTitle title = case title of
      "base" -> benchNF title
      _      -> bcompare ("$NF == \"base\" && $(NF-1) == \"" ++ groupTitle ++ "\"")
              . benchNF title

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
