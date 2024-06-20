{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

module Unicode.CharSpec
  ( spec
  ) where

import Data.Bits (Bits(..))
import qualified Data.Char as Char
import Data.Foldable (traverse_)
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
import qualified Unicode.Internal.Char.UnicodeData.GeneralCategory as UC
import Test.Hspec
-- Use to display warnings. See note in `shouldBeEqualToV`.
import Debug.Trace (traceM)

#if MIN_VERSION_base(4,15,0)
import Data.Version (showVersion)
import GHC.Unicode (unicodeVersion)

hasGhcUnicodeVersion :: Bool
hasGhcUnicodeVersion = True

#else
import Data.Version (Version, makeVersion, showVersion)

-- | Dummy Unicode version.
unicodeVersion :: Version
unicodeVersion = makeVersion [0, 0, 0]

hasGhcUnicodeVersion :: Bool
hasGhcUnicodeVersion = False
#endif

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
| 9.2.[1-6]   | 4.16.0         | 14.0            |
| 9.4.[1-4]   | 4.17.0         | 14.0            |
| 9.6.[1-3]   | 4.18.{0,1}     | 15.0            |
| 9.6.[4-5]   | 4.18.2+        | 15.1            |
| 9.8.1       | 4.19.0         | 15.1            |
| 9.10.1      | 4.20.0         | 15.1            |
+-------------+----------------+-----------------+
-}

spec :: Spec
spec = do
  describe "Unicode blocks" do
    it "Characters not in any block are unassigned"
        let { check c = case UBlocks.block c of
            Just  _ -> pure ()
            Nothing -> UChar.generalCategory c `shouldBe` UChar.NotAssigned
        } in traverse_ check [minBound..maxBound]
    it "Examples" do
        let blockDef = UBlocks.blockDefinition UBlocks.Latin1Supplement
        UBlocks.blockRange blockDef `shouldBe` (0x0080, 0x00ff)
        UBlocks.blockName  blockDef `shouldBe` "Latin-1 Supplement"
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
                      in traverse_ (checkChar b. UChar.chr) (range r);
            checkChar b c = let b' = UBlocks.block c in if b' == Just b
                then pure ()
                else expectationFailure $ mconcat
                    [ "Block is different for “", show c, "”. Expected: “Just "
                    , show b, "” but got: “", show b', "”." ]
        } in traverse_ check [minBound..maxBound]
  describe "Unicode general categories" do
    it "generalCategory" do
      -- [NOTE] We cannot compare the categories directly, so use 'show'.
      (show . UChar.generalCategory) `shouldBeEqualToV` (show . Char.generalCategory)
  describe "Character classification" do
    it "isAlpha" do
      UChar.isAlpha `shouldBeEqualToV` Char.isAlpha
    describe "isAlphaNum" do
      let isAlphaNumRef = \case
                        UChar.UppercaseLetter -> True
                        UChar.LowercaseLetter -> True
                        UChar.TitlecaseLetter -> True
                        UChar.ModifierLetter  -> True
                        UChar.OtherLetter     -> True
                        UChar.DecimalNumber   -> True
                        UChar.LetterNumber    -> True
                        UChar.OtherNumber     -> True
                        _                     -> False
      it "Check max codepoint for isAlphaNum" do
        Char.chr UC.MaxIsAlphaNum `shouldBe` maxCodePointBy isAlphaNumRef
        UC.MaxIsAlphaNum `shouldSatisfy` isPlane0To3
      it "Compare to base" do
        UChar.isAlphaNum `shouldBeEqualToV` Char.isAlphaNum
    it "isControl" do
      UChar.isControl `shouldBeEqualToV` Char.isControl
    describe "isLetter" do
      let isLetterRef = \case
                        UChar.UppercaseLetter -> True
                        UChar.LowercaseLetter -> True
                        UChar.TitlecaseLetter -> True
                        UChar.ModifierLetter  -> True
                        UChar.OtherLetter     -> True
                        _                     -> False
      it "Check max codepoint for isLetterRef" do
        Char.chr UC.MaxIsLetter `shouldBe` maxCodePointBy isLetterRef
        UC.MaxIsLetter `shouldSatisfy` isPlane0To3
      it "Compare to base" do
        UCharCompat.isLetter `shouldBeEqualToV` Char.isLetter
    it "isMark" do
      UChar.isMark `shouldBeEqualToV` Char.isMark
    it "isPrint" do
      UChar.isPrint `shouldBeEqualToV` Char.isPrint
    it "isPunctuation" do
      UChar.isPunctuation `shouldBeEqualToV` Char.isPunctuation
    describe "isSeparator" do
      it "Check max codepoint for isSeparator" do
        let isSeparatorRef = \case
                            UChar.Space              -> True
                            UChar.LineSeparator      -> True
                            UChar.ParagraphSeparator -> True
                            _                        -> False
        Char.chr UC.MaxIsSeparator `shouldBe` maxCodePointBy isSeparatorRef
        UC.MaxIsSeparator `shouldSatisfy` isPlane0To3
      it "Compare to base" do
        UChar.isSeparator `shouldBeEqualToV` Char.isSeparator
    describe "isSpace" do
      it "Check max codepoint for Space" do
        let isSpaceRef = (== UChar.Space)
        Char.chr UC.MaxIsSpace `shouldBe` maxCodePointBy isSpaceRef
        UC.MaxIsSpace `shouldSatisfy` isPlane0To3
      it "Compare to base" do
        UCharCompat.isSpace `shouldBeEqualToV` Char.isSpace
    it "isSymbol" do
      UChar.isSymbol `shouldBeEqualToV` Char.isSymbol
  describe "Case" do
    describe "isLower" do
      it "Check max codepoint for lower" do
        let isLowerRef = (== UChar.LowercaseLetter)
        Char.chr UC.MaxIsLower `shouldBe` maxCodePointBy isLowerRef
        UC.MaxIsLower `shouldSatisfy` isPlane0To3
      it "Compare to base" do
          UCharCompat.isLower `shouldBeEqualToV` Char.isLower
#if MIN_VERSION_base(4,18,0)
    it "isLowerCase" do
      UChar.isLowerCase `shouldBeEqualToV` Char.isLowerCase
#endif
    describe "isUpper" do
      it "Check max codepoint for upper" do
        let isUpperRef = \case
                            UChar.UppercaseLetter -> True
                            UChar.TitlecaseLetter -> True
                            _                     -> False
        Char.chr UC.MaxIsUpper `shouldBe` maxCodePointBy isUpperRef
        UC.MaxIsUpper `shouldSatisfy` isPlane0To3
      it "Compare to base" do
        UCharCompat.isUpper `shouldBeEqualToV` Char.isUpper
#if MIN_VERSION_base(4,18,0)
    it "isUpperCase" do
      UChar.isUpperCase `shouldBeEqualToV` Char.isUpperCase
#endif
    it "toLower" do
      UChar.toLower `shouldBeEqualToV` Char.toLower
    let caseCheck f (c, cs) = c `shouldSatisfy` (== cs) . f
    describe "toLowerString" do
        it "Examples" do
            let examples = [ ('\0', "\0")
                           , ('a', "a")
                           , ('A', "a")
                           , ('1', "1")
                           , ('\x130', "i\x307") ]
            traverse_ (caseCheck UChar.toLowerString) examples
        it "Common mapping should match simple one" do
            let check c = case UChar.toLowerString c of
                        [c'] -> c `shouldSatisfy` ((== c') . UChar.toLower)
                        _    -> pure ()
            traverse_ check [minBound..maxBound]
        it "Idempotency of 'foldMap toLowerString'" do
            let check c = c `shouldSatisfy` \c' ->
                    let cf = UChar.toLowerString c'
                    in cf == foldMap UChar.toLowerString cf
            traverse_ check [minBound..maxBound]
    it "toUpper" do
      UChar.toUpper `shouldBeEqualToV` Char.toUpper
    describe "toUpperString" do
        it "Examples" do
            let examples = [ ('\0', "\0")
                           , ('a', "A")
                           , ('A', "A")
                           , ('1', "1")
                           , ('\xdf', "SS")
                           , ('\x1F52', "\x03A5\x0313\x0300") ]
            traverse_ (caseCheck UChar.toUpperString) examples
        it "Common mapping should match simple one" do
            let check c = case UChar.toUpperString c of
                        [c'] -> c `shouldSatisfy` ((== c') . UChar.toUpper)
                        _    -> pure ()
            traverse_ check [minBound..maxBound]
        it "Idempotency of 'foldMap toUpperString'" do
            let check c = c `shouldSatisfy` \c' ->
                    let cf = UChar.toUpperString c'
                    in cf == foldMap UChar.toUpperString cf
            traverse_ check [minBound..maxBound]
    it "toTitle" do
      UChar.toTitle `shouldBeEqualToV` Char.toTitle
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
        it "Common mapping should match simple one" do
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
    describe "isNumber" do
      it "Check max codepoint for numbers" do
        let isNumber = \case
                            UChar.DecimalNumber -> True
                            UChar.LetterNumber  -> True
                            UChar.OtherNumber   -> True
                            _                   -> False
        Char.chr UC.MaxIsNumber `shouldBe` maxCodePointBy isNumber
        UC.MaxIsNumber `shouldSatisfy` isPlane0To3
      it "Compare to base" do
        UNumericCompat.isNumber `shouldBeEqualToV` Char.isNumber
    it "isNumber implies a numeric value" do
      -- [NOTE] the following does not hold with the current predicate `isNumber`.
      --        As of Unicode 15.0.0, there are 81 such characters (all CJK).
      -- let check c = (UNumeric.isNumber c `xor` isNothing (UNumeric.numericValue c))
      let check c = not (UNumericCompat.isNumber c) || isJust (UNumeric.numericValue c)
      traverse_ (`shouldSatisfy` check) [minBound..maxBound]
  where
    -- [NOTE] Unused for now
    -- shouldBeEqualTo
    --     :: forall a b. (Bounded a, Enum a, Show a, Eq b, Show b)
    --     => (a -> b)
    --     -> (a -> b)
    --     -> IO ()
    -- shouldBeEqualTo f g =
    --     let same x = f x == g x
    --     in traverse_ (`shouldSatisfy` same) [minBound..maxBound]

    -- There is no feature to display warnings other than `trace`.
    -- If we use `pendingWith` then the whole test is pending, not just
    -- the assertion.
    shouldBeEqualToV
        :: forall b. (HasCallStack) => (Eq b, Show b)
        => (Char -> b)
        -> (Char -> b)
        -> IO ()
    shouldBeEqualToV f g =
        let same x = f x == g x
        in traverse_ (`shouldSatisfyV` same) [minBound..maxBound]

    shouldSatisfyV :: (HasCallStack) => Char -> (Char -> Bool) -> IO ()
    shouldSatisfyV c h
        | hasSameUnicodeVersion = shouldSatisfy c h
        | h c = pure ()
        | not hasGhcUnicodeVersion = traceM . mconcat $
            [ "[WARNING] Cannot test ", show c
            , ": incompatible Unicode version (GHC too old). Expected "
            , showVersion UChar.unicodeVersion ]
        | isUnassigned c = traceM . mconcat $
            [ "[WARNING] Cannot test ", show c
            , ": incompatible Unicode version (unassigned char). Expected "
            , showVersion UChar.unicodeVersion
            , ", but got: "
            , showVersion unicodeVersion ]
        | hasDifferentCategory c = traceM . mconcat $
            [ "[WARNING] Cannot test ", show c
            , ": incompatible Unicode version (different general category)."
            , " Expected "
            , showVersion UChar.unicodeVersion
            , ", but got: "
            , showVersion unicodeVersion ]
        | otherwise = shouldSatisfy c h
    -- Check if the character is not assigned in exactly one Unicode version.
    isUnassigned c = (UChar.generalCategory c == UChar.NotAssigned)
               `xor` (Char.generalCategory c == Char.NotAssigned)
    -- Check if the character has changed its general category
    hasDifferentCategory c = fromEnum (UChar.generalCategory c)
                          /= fromEnum (Char.generalCategory c)
    hasSameUnicodeVersion = unicodeVersion == UChar.unicodeVersion

    -- [NOTE] Unused for now
    -- shouldBe' x y
    --     | x == y = pure ()
    --     | unicodeVersion /= UChar.unicodeVersion = traceM msg
    --     | otherwise = shouldBe x y
    --     where
    --     msg = mconcat
    --         [ "[WARNING] Cannot test ", show x
    --         , ": incompatible Unicode version. Expected "
    --         , showVersion UChar.unicodeVersion
    --         , ", but got: "
    --         , unicodeVersionString ]

    isPlane0To3 = (< 0x40000)
    maxCodePointBy p = foldr
        (\c -> if p (UChar.generalCategory c) then max c else id)
        minBound
        [minBound..maxBound]
