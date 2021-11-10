{-# LANGUAGE BlockArguments #-}

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

* GHC 8.10 = Unicode 13.0
-}

spec :: Spec
spec = do
  describe "Unicode general categories" do
    it "generalCategory" do
      -- Note: we cannot compare the categories directly, so use 'show'.
      let sameGC c = show (UChar.generalCategory c) == show (Char.generalCategory c)
      traverse_ (`shouldSatisfy` sameGC) [minBound..maxBound]
  describe "Character classification" do
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
