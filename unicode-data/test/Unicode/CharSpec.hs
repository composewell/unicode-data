{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

module Unicode.CharSpec
  ( spec
  ) where

import qualified Data.Char as Char
import Data.Ix (Ix(..))
import Data.Maybe (isJust)
import qualified Unicode.Char as UChar
import qualified Unicode.Char.General.Blocks as UBlocks
-- [TODO] Remove the following qualified imports once isLetter and isSpace
--        are removed from Unicode.Char.General
import qualified Unicode.Char.General.Compat as UCharCompat
-- [TODO] Remove the following qualified imports once isUpper and isLower
--        are removed from Unicode.Char.Case
import qualified Unicode.Char.Case.Compat as UCharCompat
import qualified Unicode.Char.Numeric as UNumeric
import qualified Unicode.Char.Numeric.Compat as UNumericCompat
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
| 9.0.[1-2]   | 4.15.0         | 12.1            |
| 9.2.[1-4]   | 4.16.0         | 14.0            |
| 9.4.[1-2]   | 4.17.0         | 14.0            |
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
  describe "Unicode blocks" do
    it "Characters not in any block are unassigned"
        let { check c = case UBlocks.block c of
            Just  _ -> pure ()
            Nothing -> UChar.generalCategory c `shouldBe` UChar.NotAssigned
        } in traverse_ check [minBound..maxBound]
    it "Characters are in the definition of their corresponding block"
        let {
            check c = case UBlocks.block c of
                Nothing -> pure ()
                Just b  ->
                    let r = UBlocks.blockRange (UBlocks.blockDefinition b)
                    in if inRange r (UChar.ord c)
                        then pure ()
                        else expectationFailure $ mconcat
                            [ "Character “", show c
                            , "” is not in the block “", show b, "”." ]
        } in traverse_ check [minBound..maxBound]
    it "Characters in a block definition have the corresponding block"
        let {
            check b = let r = UBlocks.blockRange (UBlocks.blockDefinition b)
                      in traverse_ (checkChar b) (UChar.chr <$> range r);
            checkChar b c = let b' = UBlocks.block c in if b' == Just b
                then pure ()
                else expectationFailure $ mconcat
                    [ "Block is different for “", show c, "”. Expected: “Just "
                    , show b, "” but got: “", show b', "”." ]
        } in traverse_ check [minBound..maxBound]
  describe' "Unicode general categories" do
    it "generalCategory" do
      -- [NOTE] We cannot compare the categories directly, so use 'show'.
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
  describe "Case" do
    it' "isLower" do
      UCharCompat.isLower `shouldBeEqualTo` Char.isLower
    it' "isUpper" do
      UCharCompat.isUpper `shouldBeEqualTo` Char.isUpper
    it' "toLower" do
      UChar.toLower `shouldBeEqualTo` Char.toLower
    let caseCheck f (c, cs) = c `shouldSatisfy` (== cs) . f
    describe "toLowerString" do
        it "Examples" do
            let examples = [ ('\0', "\0")
                           , ('a', "a")
                           , ('A', "a")
                           , ('1', "1")
                           , ('\x130', "i\x307") ]
            traverse_ (caseCheck UChar.toLowerString) examples
        it' "Common mapping should match simple one" do
            let check c = case UChar.toLowerString c of
                        [c'] -> c `shouldSatisfy` ((== c') . UChar.toLower)
                        _    -> pure ()
            traverse_ check [minBound..maxBound]
        it "Idempotency of 'foldMap toLowerString'" do
            let check c = c `shouldSatisfy` \c' ->
                    let cf = UChar.toLowerString c'
                    in cf == foldMap UChar.toLowerString cf
            traverse_ check [minBound..maxBound]
    it' "toUpper" do
      UChar.toUpper `shouldBeEqualTo` Char.toUpper
    describe "toUpperString" do
        it "Examples" do
            let examples = [ ('\0', "\0")
                           , ('a', "A")
                           , ('A', "A")
                           , ('1', "1")
                           , ('\xdf', "SS")
                           , ('\x1F52', "\x03A5\x0313\x0300") ]
            traverse_ (caseCheck UChar.toUpperString) examples
        it' "Common mapping should match simple one" do
            let check c = case UChar.toUpperString c of
                        [c'] -> c `shouldSatisfy` ((== c') . UChar.toUpper)
                        _    -> pure ()
            traverse_ check [minBound..maxBound]
        it "Idempotency of 'foldMap toUpperString'" do
            let check c = c `shouldSatisfy` \c' ->
                    let cf = UChar.toUpperString c'
                    in cf == foldMap UChar.toUpperString cf
            traverse_ check [minBound..maxBound]
    it' "toTitle" do
      UChar.toTitle `shouldBeEqualTo` Char.toTitle
    describe "toTitleString" do
        it "Examples" do
            let examples = [ ('\0', "\0")
                           , ('a', "A")
                           , ('A', "A")
                           , ('1', "1")
                           , ('\xdf', "Ss")
                           , ('\xfb02', "Fl")
                           , ('\x1F52', "\x03A5\x0313\x0300") ]
            traverse_ (caseCheck UChar.toTitleString) examples
        it' "Common mapping should match simple one" do
            let check c = case UChar.toTitleString c of
                        [c'] -> c `shouldSatisfy` ((== c') . UChar.toTitle)
                        _    -> pure ()
            traverse_ check [minBound..maxBound]
    describe "toCaseFoldString" do
        it "Examples" do
            let examples = [ ('\0', "\0")
                           , ('a', "a")
                           , ('A', "a")
                           , ('1', "1")
                           , ('\xb5', "\x3bc")
                           , ('\xfb13', "\x574\x576") ]
            traverse_ (caseCheck UChar.toCaseFoldString) examples
        it "Idempotency of 'foldMap toCaseFoldString'" do
            let check c = c `shouldSatisfy` \c' ->
                    let cf = UChar.toCaseFoldString c'
                    in cf == foldMap UChar.toCaseFoldString cf
            traverse_ check [minBound..maxBound]
  describe "Numeric" do
    it' "isNumber" do
      UNumericCompat.isNumber `shouldBeEqualTo` Char.isNumber
    it "isNumber implies a numeric value" do
      -- [NOTE] the following does not hold with the current predicate `isNumber`.
      --        As of Unicode 14.0.0, there are 81 such characters (all CJK).
      -- 'let check c = (UNumeric.isNumber c `xor` isNothing (UNumeric.numericValue c))
      let check c = not (UNumericCompat.isNumber c) || isJust (UNumeric.numericValue c)
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
