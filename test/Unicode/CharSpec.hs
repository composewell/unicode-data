{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

module Unicode.CharSpec
  ( spec
  ) where

import qualified Data.Char as Char
import qualified Unicode.Char as UChar
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
#else
  let describe' t = before_ (pendingWith "Incompatible GHC Unicode version")
                  . describe t
#endif
  describe' "Unicode general categories" do
    it "generalCategory" do
      -- Note: we cannot compare the categories directly, so use 'show'.
      (show . UChar.generalCategory) `shouldBeEqualTo` (show . Char.generalCategory)
  describe' "Character classification" do
    it "isAlpha" do
      UChar.isAlpha `shouldBeEqualTo` Char.isAlpha
    it "isAlphaNum" do
      UChar.isAlphaNum `shouldBeEqualTo` Char.isAlphaNum
    it "isControl" do
      UChar.isControl `shouldBeEqualTo` Char.isControl
    it "isLetter" do
      UChar.isLetter `shouldBeEqualTo` Char.isLetter
    it "isMark" do
      UChar.isMark `shouldBeEqualTo` Char.isMark
    it "isNumber" do
      UChar.isNumber `shouldBeEqualTo` Char.isNumber
    it "isPrint" do
      UChar.isPrint `shouldBeEqualTo` Char.isPrint
    it "isPunctuation" do
      UChar.isPunctuation `shouldBeEqualTo` Char.isPunctuation
    it "isSeparator" do
      UChar.isSeparator `shouldBeEqualTo` Char.isSeparator
    it "isSpace" do
      UChar.isSpace `shouldBeEqualTo` Char.isSpace
    it "isSymbol" do
      UChar.isSymbol `shouldBeEqualTo` Char.isSymbol
  describe' "Case" do
    it "isLower" do
      UChar.isLower `shouldBeEqualTo` Char.isLower
    it "isUpper" do
      UChar.isUpper `shouldBeEqualTo` Char.isUpper
    it "toLower" do
      UChar.toLower `shouldBeEqualTo` Char.toLower
    it "toUpper" do
      UChar.toUpper `shouldBeEqualTo` Char.toUpper
    it "toTitle" do
      UChar.toTitle `shouldBeEqualTo` Char.toTitle
  where
    shouldBeEqualTo
        :: forall a b. (Bounded a, Enum a, Show a, Eq b, Show b)
        => (a -> b)
        -> (a -> b)
        -> IO ()
    shouldBeEqualTo f g =
        let same x = f x == g x
        in traverse_ (`shouldSatisfy` same) [minBound..maxBound]