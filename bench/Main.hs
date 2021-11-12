import Data.Ix (Ix(..))
import Control.DeepSeq (NFData, deepseq)
import Test.Tasty.Bench

import qualified Data.Char as B
import qualified Unicode.Char.Case as C
import qualified Unicode.Char.General as G
import qualified Unicode.Char.Identifiers as I
import qualified Unicode.Char.Normalization as N

main :: IO ()
main = defaultMain
  [ bgroup "Unicode.Char.Case"
    [ bgroup "isLower"
      [ bench "base"         $ nf (fold_ B.isLower) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ C.isLower) (minBound, maxBound)
      ]
    , bgroup "isUpper"
      [ bench "base"         $ nf (fold_ B.isUpper) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ C.isUpper) (minBound, maxBound)
      ]
    ]
  , bgroup "Unicode.Char.General"
    -- Character classification
    [ bgroup "generalCategory"
      [ bench "base"         $ nf (fold_ (show . B.generalCategory)) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ (show . G.generalCategory)) (minBound, maxBound)
      ]
    , bgroup "isAlpa"
      [ bench "base"         $ nf (fold_ B.isAlpha) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isAlpha) (minBound, maxBound)
      ]
    , bgroup "isAlpabetic"
      [ bench "unicode-data" $ nf (fold_ G.isAlphabetic) (minBound, maxBound)
      ]
    , bgroup "isAlpaNum"
      [ bench "base"         $ nf (fold_ B.isAlphaNum) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isAlphaNum) (minBound, maxBound)
      ]
    , bgroup "isControl"
      [ bench "base"         $ nf (fold_ B.isControl) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isControl) (minBound, maxBound)
      ]
    , bgroup "isLetter"
      [ bench "base"         $ nf (fold_ G.isLetter) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isLetter) (minBound, maxBound)
      ]
    , bgroup "isMark"
      [ bench "base"         $ nf (fold_ B.isMark) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isMark) (minBound, maxBound)
      ]
    , bgroup "isNumber"
      [ bench "base"         $ nf (fold_ B.isNumber) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isNumber) (minBound, maxBound)
      ]
    , bgroup "isPrint"
      [ bench "base"         $ nf (fold_ B.isPrint) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isPrint) (minBound, maxBound)
      ]
    , bgroup "isPunctuation"
      [ bench "base"         $ nf (fold_ B.isPunctuation) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isPunctuation) (minBound, maxBound)
      ]
    , bgroup "isSeparator"
      [ bench "base"         $ nf (fold_ B.isSeparator) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isSeparator) (minBound, maxBound)
      ]
    , bgroup "isSpace"
      [ bench "base"         $ nf (fold_ G.isSpace) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isSpace) (minBound, maxBound)
      ]
    , bgroup "isSymbol"
      [ bench "base"         $ nf (fold_ B.isSymbol) (minBound, maxBound)
      , bench "unicode-data" $ nf (fold_ G.isSymbol) (minBound, maxBound)
      ]
    , bgroup "isWhiteSpace"
      [ bench "unicode-data" $ nf (fold_ G.isWhiteSpace) (minBound, maxBound)
      ]
    -- Korean Hangul Characters
    , bgroup "isHangul"
      [ bench "unicode-data" $ nf (fold_ G.isHangul) (minBound, maxBound)
      ]
    , bgroup "isHangulLV"
      [ bench "unicode-data" $ nf (fold_ G.isHangul) (minBound, maxBound)
      ]
    , bgroup "isJamo"
      [ bench "unicode-data" $ nf (fold_ G.isJamo) (minBound, maxBound)
      ]
    , bgroup "jamoLIndex"
      [ bench "unicode-data" $ nf (fold_ G.jamoLIndex) (minBound, maxBound)
      ]
    , bgroup "jamoVIndex"
      [ bench "unicode-data" $ nf (fold_ G.jamoVIndex) (minBound, maxBound)
      ]
    , bgroup "jamoTIndex"
      [ bench "unicode-data" $ nf (fold_ G.jamoTIndex) (minBound, maxBound)
      ]
    ]
  , bgroup "Unicode.Char.Identifiers"
    [ bgroup "isIDContinue"
      [ bench "unicode-data" $ nf (fold_ I.isIDContinue) (minBound, maxBound)
      ]
    , bgroup "isIDStart"
      [ bench "unicode-data" $ nf (fold_ I.isIDStart) (minBound, maxBound)
      ]
    , bgroup "isXIDContinue"
      [ bench "unicode-data" $ nf (fold_ I.isXIDContinue) (minBound, maxBound)
      ]
    , bgroup "isXIDStart"
      [ bench "unicode-data" $ nf (fold_ I.isXIDStart) (minBound, maxBound)
      ]
    , bgroup "isPatternSyntax"
      [ bench "unicode-data" $ nf (fold_ I.isPatternSyntax) (minBound, maxBound)
      ]
    , bgroup "isPatternWhitespace"
      [ bench "unicode-data" $ nf (fold_ I.isPatternWhitespace) (minBound, maxBound)
      ]
    ]
  , bgroup "Unicode.Char.Normalization"
    [ bgroup "isCombining"
      [ bench "unicode-data" $ nf (fold_ N.isCombining) (minBound, maxBound)
      ]
    , bgroup "combiningClass"
      [ bench "unicode-data" $ nf (fold_ N.combiningClass) (minBound, maxBound)
      ]
    , bgroup "isCombiningStarter"
      [ bench "unicode-data" $ nf (fold_ N.isCombiningStarter) (minBound, maxBound)
      ]
    -- [TODO] compose, composeStarters
    , bgroup "isDecomposable"
      [ bgroup "Canonical"
        [ bench "unicode-data" $ nf (fold_ (N.isDecomposable N.Canonical)) (minBound, maxBound)
        ]
      , bgroup "Kompat"
        [ bench "unicode-data" $ nf (fold_ (N.isDecomposable N.Kompat)) (minBound, maxBound)
        ]
      ]
    -- [FIXME] Fail due to non-exhaustive pattern matching
    -- , bgroup "decompose"
    --   [ bgroup "Canonical"
    --     [ bench "unicode-data" $ nf (fold_ (N.decompose N.Canonical)) (minBound, maxBound)
    --     ]
    --   , bgroup "Kompat"
    --     [ bench "unicode-data" $ nf (fold_ (N.decompose N.Kompat)) (minBound, maxBound)
    --     ]
    --   ]
    , bgroup "decomposeHangul"
      [ bench "unicode-data" $ nf (fold_ N.decomposeHangul) (minBound, maxBound)
      ]
    ]
  ]
  where
    fold_ :: forall a. (NFData a) => (Char -> a) -> (Char, Char) -> ()
    fold_ f = foldr (deepseq . f) () . range