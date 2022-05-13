{-# LANGUAGE BlockArguments #-}
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
import qualified Unicode.Char.Numeric as UNumeric
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
  let it' = before_ (pendingWith "Incompatible GHC Unicode version")
          . it t
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
      UCharCompat.isLetter `shouldBeEqualTo` Char.isLetter
    it "isMark" do
      UChar.isMark `shouldBeEqualTo` Char.isMark
    it "isPrint" do
      UChar.isPrint `shouldBeEqualTo` Char.isPrint
    it "isPunctuation" do
      UChar.isPunctuation `shouldBeEqualTo` Char.isPunctuation
    it "isSeparator" do
      UChar.isSeparator `shouldBeEqualTo` Char.isSeparator
    it "isSpace" do
      UCharCompat.isSpace `shouldBeEqualTo` Char.isSpace
    it "isSymbol" do
      UChar.isSymbol `shouldBeEqualTo` Char.isSymbol
  describe' "Case" do
    it "isLower" do
      UCharCompat.isLower `shouldBeEqualTo` Char.isLower
    it "isUpper" do
      UCharCompat.isUpper `shouldBeEqualTo` Char.isUpper
    it "toLower" do
      UChar.toLower `shouldBeEqualTo` Char.toLower
    it "toUpper" do
      UChar.toUpper `shouldBeEqualTo` Char.toUpper
    it "toTitle" do
      UChar.toTitle `shouldBeEqualTo` Char.toTitle
  describe "Numeric" do
    it' "isNumber" do
      UNumeric.isNumber `shouldBeEqualTo` Char.isNumber
    it "isNumber implies a numeric value" do
      -- [NOTE] the following does not hold with the current predicate `isNumber`.
      -- 'let check c = (UNumeric.isNumber c `xor` isNothing (UNumeric.numericValue c))
      let check c = not (UNumeric.isNumber c) || isJust (UNumeric.numericValue c)
      traverse_ (`shouldSatisfy` check) [minBound..maxBound]
  where
    shouldBeEqualTo
        :: forall a b. (Bounded a, Enum a, Show a, Eq b, Show b)
        => (a -> b)
        -> (a -> b)
        -> IO ()
    shouldBeEqualTo f g =
        let same x = f x == g x
        in traverse_ (`shouldSatisfy` same) [minBound..maxBound]