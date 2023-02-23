{-# LANGUAGE CPP, BlockArguments #-}

module Unicode.Text.CaseSpec
  ( spec
  ) where

import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Unicode.Text.Case as C
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
| 9.2.[1-6]   | 4.16.0         | 14.0            |
| 9.4.[1-4]   | 4.17.0         | 14.0            |
| 9.6.1       | 4.18.0         | 15.0            |
+-------------+----------------+-----------------+
-}

spec :: Spec
spec = do
#ifndef COMPATIBLE_GHC_UNICODE
    let it' t = before_ (pendingWith "Incompatible GHC Unicode version") . it t
#elif !MIN_VERSION_text(2,0,1)
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
            C.toLowerFusion cs `shouldBe` T.toLower cs
#if MIN_VERSION_text(2,0,0)
            C.toLower cs `shouldBe` T.toLower cs
#endif
    describe "toUpper" do
        it "Idempotent" do
            let cs' = C.toUpperFusion cs in cs' `shouldBe` C.toUpperFusion cs'
#if MIN_VERSION_text(2,0,0)
            let cs' = C.toUpper cs in cs' `shouldBe` C.toUpper cs'
#endif
        it' "Compare with `text`" do
            C.toUpperFusion cs `shouldBe` T.toUpper cs
#if MIN_VERSION_text(2,0,0)
            C.toUpper cs `shouldBe` T.toUpper cs
#endif
    describe "toTitle" do
        it' "Compare with `text`" do
            let cmpTitle c = let t = T.pack [c, 'a', ' ', 'a', c, ' ']
                             in C.toTitleFusion t == T.toTitle t
            let check = (`shouldSatisfy` cmpTitle)
            traverse_ check [minBound..maxBound]
    describe "toCaseFold" do
        it "Idempotent" do
            let cs' = C.toCaseFoldFusion cs in cs' `shouldBe` C.toCaseFoldFusion cs'
#if MIN_VERSION_text(2,0,0)
            let cs' = C.toCaseFold cs in cs' `shouldBe` C.toCaseFold cs'
#endif
        it' "Compare with `text`" do
            C.toCaseFoldFusion cs `shouldBe` T.toCaseFold cs
#if MIN_VERSION_text(2,0,0)
            C.toCaseFold cs `shouldBe` T.toCaseFold cs
#endif


