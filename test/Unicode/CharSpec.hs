{-# LANGUAGE CPP #-}

module Unicode.CharSpec
  ( spec
  ) where

import qualified Data.Char as Char
import Data.Maybe (isJust)
import qualified Unicode.Char as UChar
-- [TODO] Remove the following qualified imports once isLetter and isSpace
--        are removed from Unicode.Char.General
import qualified Unicode.Char.General.Compat as UCharCompat
-- [TODO] Remove the following qualified imports once isUpper and isLower
--        are removed from Unicode.Char.Case
import qualified Unicode.Char.Case.Compat as UCharCompat
import qualified Unicode.Char.Normalization as UNorm
import qualified Unicode.Char.Numeric as UNumeric
import qualified Unicode.Char.Numeric.Compat as UNumericCompat
import qualified Unicode.Internal.Char.DerivedNormalizationProperties as UINorm
import Data.Foldable (traverse_)
import Test.Hspec

{- [NOTE]
These tests may fail if the compiler’s Unicode version
does not match the version of this package.

+-------------+----------------+-----------------+
| GHC version | @base@ version | Unicode version |
+=============+================+=================+
| 8.8         | 4.13           | 12.0            |
| 8.10.[1-4]  | 4.14.{0,1}     | 12.0            |
| 8.10.5+     | 4.14.2+        | 13.0            |
| 9.0.1       | 4.15.0         | 12.1            |
| 9.2.1       | 4.16.0         | 14.0            |
+-------------+----------------+-----------------+
-}

spec :: Spec
spec = do
#ifdef COMPATIBLE_GHC_UNICODE
  let describe' = describe
  let it' = it
#else
  let describe' t = before_ (pendingWith "Incompatible GHC Unicode version")
                  . describe t
  let it' t = before_ (pendingWith "Incompatible GHC Unicode version")
            . it t
#endif
  describe' "Unicode general categories" $
    it "generalCategory" $
      -- [NOTE] We cannot compare the categories directly, so use 'show'.
      (show . UChar.generalCategory) `shouldBeEqualTo` (show . Char.generalCategory)
  describe' "Character classification" $ do
    it "isAlpha" $
      UChar.isAlpha `shouldBeEqualTo` Char.isAlpha
    it "isAlphaNum" $
      UChar.isAlphaNum `shouldBeEqualTo` Char.isAlphaNum
    it "isControl" $
      UChar.isControl `shouldBeEqualTo` Char.isControl
    it "isLetter" $
      UCharCompat.isLetter `shouldBeEqualTo` Char.isLetter
    it "isMark" $
      UChar.isMark `shouldBeEqualTo` Char.isMark
    it "isPrint" $
      UChar.isPrint `shouldBeEqualTo` Char.isPrint
    it "isPunctuation" $
      UChar.isPunctuation `shouldBeEqualTo` Char.isPunctuation
    it "isSeparator" $
      UChar.isSeparator `shouldBeEqualTo` Char.isSeparator
    it "isSpace" $
      UCharCompat.isSpace `shouldBeEqualTo` Char.isSpace
    it "isSymbol" $
      UChar.isSymbol `shouldBeEqualTo` Char.isSymbol
  describe' "Case" $ do
    it "isLower" $
      UCharCompat.isLower `shouldBeEqualTo` Char.isLower
    it "isUpper" $
      UCharCompat.isUpper `shouldBeEqualTo` Char.isUpper
    it "toLower" $
      UChar.toLower `shouldBeEqualTo` Char.toLower
    it "toUpper" $
      UChar.toUpper `shouldBeEqualTo` Char.toUpper
    it "toTitle" $
      UChar.toTitle `shouldBeEqualTo` Char.toTitle
  describe "Normalization" $ do
    let {
        checkCompose isNF isNFInt = \c -> case isNFInt c of
        0 -> isNF c `shouldBe` UNorm.No
        1 -> isNF c `shouldBe` UNorm.Maybe
        2 -> do
            isNF c `shouldBe` UNorm.Yes
            c `shouldSatisfy` UChar.isCombining
        3 -> do
            isNF c `shouldBe` UNorm.Yes
            c `shouldSatisfy` not . UChar.isCombining
        n -> error ("Unexpected value: " ++ show n)
    }
    it "isNFC_QC" $
      let check = checkCompose UNorm.isNFC UINorm.isNFC_QC
      in traverse_ check [minBound..maxBound]
    it "isNFKC_QC" $
      let check = checkCompose UNorm.isNFKC UINorm.isNFKC_QC
      in traverse_ check [minBound..maxBound]
    it "isNFKD" $
      let {
        check c = if UNorm.isNFKD c
            then do
                c `shouldNotSatisfy` UNorm.isDecomposable UNorm.Kompat
                c `shouldNotSatisfy` UChar.isHangul
            else do
                c `shouldSatisfy` (\c' ->
                    UNorm.isDecomposable UNorm.Kompat c' ||
                    UChar.isHangul c')
      } in traverse_ check [minBound..maxBound]
    it "isNFD c == No => isNFKD c == No" $
      let {
        check c = if UNorm.isNFD c
            then do
                c `shouldNotSatisfy` UNorm.isDecomposable UNorm.Canonical
                c `shouldNotSatisfy` UChar.isHangul
            else do
                c `shouldNotSatisfy` UNorm.isNFKD
                c `shouldSatisfy` (\c' ->
                    UNorm.isDecomposable UNorm.Canonical c' ||
                    UChar.isHangul c')
      } in traverse_ check [minBound..maxBound]
    it "isNFC c == No => isNFKC c == No" $
      let check c = case UNorm.isNFC c of
                    UNorm.No -> UNorm.isNFKC c `shouldBe` UNorm.No
                    _        -> pure ()
      in traverse_ check [minBound..maxBound]
    it "isNFC c == No => isNFD c == No" $
      let check c = case UNorm.isNFC c of
                    UNorm.No -> UNorm.isNFD c `shouldBe` False
                    _        -> pure ()
      in traverse_ check [minBound..maxBound]
    it "isNFC c == No => isNFKD c == No" $
      let check c = case UNorm.isNFC c of
                    UNorm.No -> UNorm.isNFKD c `shouldBe` False
                    _        -> pure ()
      in traverse_ check [minBound..maxBound]
  describe "Numeric" $ do
    it' "isNumber" $
      UNumericCompat.isNumber `shouldBeEqualTo` Char.isNumber
    it "isNumber implies a numeric value" $
      -- [NOTE] the following does not hold with the current predicate `isNumber`.
      --        As of Unicode 14.0.0, there are 81 such characters (all CJK).
      -- 'let check c = (UNumeric.isNumber c `xor` isNothing (UNumeric.numericValue c))
      let check c = not (UNumericCompat.isNumber c) || isJust (UNumeric.numericValue c)
      in traverse_ (`shouldSatisfy` check) [minBound..maxBound]
  where
    shouldBeEqualTo
        :: forall a b. (Bounded a, Enum a, Show a, Eq b, Show b)
        => (a -> b)
        -> (a -> b)
        -> IO ()
    shouldBeEqualTo f g =
        let same x = f x == g x
        in traverse_ (`shouldSatisfy` same) [minBound..maxBound]
