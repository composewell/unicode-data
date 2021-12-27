import Control.DeepSeq (NFData, deepseq)
import Data.Ix (Ix(..))
import Test.Tasty.Bench (Benchmark, bgroup, bench, bcompare, nf, defaultMain)

import qualified Data.Char as B
import qualified Unicode.Char.Case as C
import qualified Unicode.Char.Case.Compat as CC
import qualified Unicode.Char.General as G
import qualified Unicode.Char.General.Compat as GC
import qualified Unicode.Char.Identifiers as I
import qualified Unicode.Char.Normalization as N
import qualified Unicode.Char.Numeric as Num

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
    ]
  , bgroup "Unicode.Char.Case.Compat"
    [ bgroup' "isLower"
      [ Bench "base"         B.isLower
      , Bench "unicode-data" CC.isLower
      ]
    , bgroup' "isUpper"
      [ Bench "base"         B.isUpper
      , Bench "unicode-data" CC.isUpper
      ]
    , bgroup' "toLower"
      [ Bench "base"         B.toLower
      , Bench "unicode-data" CC.toLower
      ]
    , bgroup' "toTitle"
      [ Bench "base"         B.toTitle
      , Bench "unicode-data" CC.toTitle
      ]
    , bgroup' "toUpper"
      [ Bench "base"         B.toUpper
      , Bench "unicode-data" CC.toUpper
      ]
    ]
  , bgroup "Unicode.Char.General"
    -- Character classification
    [ bgroup' "generalCategory"
      [ Bench "base"          (show . B.generalCategory)
      , Bench "unicode-data"  (show . G.generalCategory)
      ]
    , bgroup "isAlphabetic"
      [ benchNF "unicode-data"  G.isAlphabetic
      ]
    , bgroup' "isAlphaNum"
      [ Bench "base"          B.isAlphaNum
      , Bench "unicode-data"  G.isAlphaNum
      ]
    , bgroup' "isControl"
      [ Bench "base"          B.isControl
      , Bench "unicode-data"  G.isControl
      ]
    , bgroup' "isMark"
      [ Bench "base"          B.isMark
      , Bench "unicode-data"  G.isMark
      ]
    , bgroup' "isPrint"
      [ Bench "base"          B.isPrint
      , Bench "unicode-data"  G.isPrint
      ]
    , bgroup' "isPunctuation"
      [ Bench "base"          B.isPunctuation
      , Bench "unicode-data"  G.isPunctuation
      ]
    , bgroup' "isSeparator"
      [ Bench "base"          B.isSeparator
      , Bench "unicode-data"  G.isSeparator
      ]
    , bgroup' "isSymbol"
      [ Bench "base"          B.isSymbol
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
  , bgroup "Unicode.Char.General.Compat"
    [ bgroup' "isAlpha"
      [ Bench "base"          B.isAlpha
      , Bench "unicode-data"  GC.isAlpha
      ]
    , bgroup' "isLetter"
      [ Bench "base"          B.isLetter
      , Bench "unicode-data"  GC.isLetter
      ]
    , bgroup' "isSpace"
      [ Bench "base"          B.isSpace
      , Bench "unicode-data"  GC.isSpace
      ]
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
    [ bgroup' "isNumber"
      [ Bench "base"          B.isNumber
      , Bench "unicode-data"  Num.isNumber
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
    benchNF t f = bench t $ nf (fold_ f) (minBound, maxBound)

    fold_ :: forall a. (NFData a) => (Char -> a) -> (Char, Char) -> ()
    fold_ f = foldr (deepseq . f) () . range