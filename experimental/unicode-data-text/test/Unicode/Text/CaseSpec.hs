{-# LANGUAGE CPP, BlockArguments #-}

module Unicode.Text.CaseSpec
  ( spec
  ) where

import Data.Bits (Bits(..))
import qualified Data.Char as Char
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Unicode.Char as UChar
import qualified Unicode.Text.Case as C
import Test.Hspec
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

+----------------+-----------------+
| `text` version | Unicode version |
+================+=================+
| 1.2.5.0        | 13.0            |
| 2.0.[0-2]      | 14.0            |
| 2.1.[0-1]      | 14.0            |
+----------------+-----------------+
-}

compatibleTextVersion :: Bool
-- Dummy test for now. Update once `text` has a compatible version with unicode-data
#if false
#define COMPATIBLE_TEXT
compatibleTextVersion = True
#else
compatibleTextVersion = False
#endif

spec :: Spec
spec = do
#if !MIN_VERSION_text(2,0,1)
    let it' t = before_ (pendingWith "Incompatible: required text >= 2.0.1")
              . it t
#else
    let it' = it
#endif
    let cs = T.pack [minBound..maxBound]
    describe "toLower" do
        it "Idempotent" do
            let cs' = C.toLowerFusion cs in cs' `shouldBe` C.toLowerFusion cs'
#if MIN_VERSION_text(2,0,0)
            let cs' = C.toLower cs in cs' `shouldBe` C.toLower cs'
#endif
        it' "Compare with `text`" do
            C.toLowerFusion cs `shouldBeV` T.toLower cs
#if MIN_VERSION_text(2,0,0)
            C.toLower cs `shouldBeV` T.toLower cs
#endif
    describe "toUpper" do
        it "Idempotent" do
            let cs' = C.toUpperFusion cs in cs' `shouldBe` C.toUpperFusion cs'
#if MIN_VERSION_text(2,0,0)
            let cs' = C.toUpper cs in cs' `shouldBe` C.toUpper cs'
#endif
        it' "Compare with `text`" do
            C.toUpperFusion cs `shouldBeV` T.toUpper cs
#if MIN_VERSION_text(2,0,0)
            C.toUpper cs `shouldBeV` T.toUpper cs
#endif
    describe "toTitle" do
        it' "Compare with `text`" do
            let cmpTitle c = let t = T.pack [c, 'a', ' ', 'a', c, ' ']
                             in C.toTitleFusion t == T.toTitle t
            let check = (`shouldSatisfyV` cmpTitle)
            traverse_ check [minBound..maxBound]
    describe "toCaseFold" do
        it "Idempotent" do
            let cs' = C.toCaseFoldFusion cs in cs' `shouldBe` C.toCaseFoldFusion cs'
#if MIN_VERSION_text(2,0,0)
            let cs' = C.toCaseFold cs in cs' `shouldBe` C.toCaseFold cs'
#endif
        it' "Compare with `text`" do
            C.toCaseFoldFusion cs `shouldBeV` T.toCaseFold cs
#if MIN_VERSION_text(2,0,0)
            C.toCaseFold cs `shouldBeV` T.toCaseFold cs
#endif
    where

    hasSameUnicodeVersion = unicodeVersion == UChar.unicodeVersion
    -- Check if the character is not assigned in exactly one Unicode version.
    isUnassigned c = (UChar.generalCategory c == UChar.NotAssigned)
               `xor` (Char.generalCategory c == Char.NotAssigned)
    -- Check if the character has changed its general category
    hasDifferentCategory c = fromEnum (UChar.generalCategory c)
                          /= fromEnum (Char.generalCategory c)

    shouldSatisfyV :: (HasCallStack) => Char -> (Char -> Bool) -> IO ()
    shouldSatisfyV c h
        | hasSameUnicodeVersion && compatibleTextVersion = shouldSatisfy c h
        | h c = pure ()
        | not hasGhcUnicodeVersion = traceM . mconcat $
            [ "[WARNING] Cannot test ", show c
            , ": incompatible Unicode version (base: GHC too old). Expected "
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
        | not compatibleTextVersion = traceM . mconcat $
            [ "[WARNING] Cannot test ", show c
            , ": incompatible Unicode version (text)."
            , " Expected "
            , showVersion UChar.unicodeVersion ]
        | otherwise = shouldSatisfy c h
    shouldBeV x y
        | hasSameUnicodeVersion && compatibleTextVersion = shouldBe x y
        | x == y = pure ()
        | not hasGhcUnicodeVersion = traceM . mconcat $
            [ "[WARNING] Cannot test ", show x, " == ", show y
            , ": incompatible Unicode version (GHC too old). Expected "
            , showVersion UChar.unicodeVersion ]
        | not compatibleTextVersion = traceM . mconcat $
            [ "[WARNING] Cannot test ", show x, " == ", show y
            , ": incompatible Unicode version (text)." ]
        | otherwise = shouldBe x y
