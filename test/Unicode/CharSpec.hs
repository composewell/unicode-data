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
  describe "Unicode general categories" do
#else
  let xdescribe' t = before_ (pendingWith "Incompatible GHC Unicode version") . describe t
  xdescribe' "Unicode general categories" do
#endif
    it "generalCategory" do
      -- Note: we cannot compare the categories directly, so use 'show'.
      let sameGC c = show (UChar.generalCategory c) == show (Char.generalCategory c)
      traverse_ (`shouldSatisfy` sameGC) [minBound..maxBound]
#ifdef COMPATIBLE_GHC_UNICODE
  describe "Character classification" do
#else
  xdescribe' "Character classification" do
#endif
    it "isAlpha" do
      let same c = UChar.isAlpha c == Char.isAlpha c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    it "isAlphaNum" do
      let same c = UChar.isAlphaNum c == Char.isAlphaNum c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    it "isControl" do
      let same c = UChar.isControl c == Char.isControl c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    it "isLetter" do
      let same c = UChar.isLetter c == Char.isLetter c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    it "isMark" do
      let same c = UChar.isMark c == Char.isMark c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    it "isNumber" do
      let same c = UChar.isNumber c == Char.isNumber c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    it "isPrint" do
      let same c = UChar.isPrint c == Char.isPrint c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    it "isPunctuation" do
      let same c = UChar.isPunctuation c == Char.isPunctuation c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    it "isSeparator" do
      let same c = UChar.isSeparator c == Char.isSeparator c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    it "isSpace" do
      let same c = UChar.isSpace c == Char.isSpace c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    it "isSymbol" do
      let same c = UChar.isSymbol c == Char.isSymbol c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
#ifdef COMPATIBLE_GHC_UNICODE
  describe "Case" do
#else
  xdescribe' "Case" do
#endif
    let xit' t = before_ (pendingWith "Incompatible implementation with Data.Char") . it t
    xit' "isLower" do
      let same c = UChar.isLower c == Char.isLower c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]
    xit' "isUpper" do
      let same c = UChar.isUpper c == Char.isUpper c
      traverse_ (`shouldSatisfy` same) [minBound..maxBound]