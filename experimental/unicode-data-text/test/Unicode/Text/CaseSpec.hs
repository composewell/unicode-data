{-# LANGUAGE CPP, BlockArguments #-}

module Unicode.Text.CaseSpec
  ( spec
  ) where

import qualified Data.Text as T
import qualified Unicode.Text.Case as C
import Test.Hspec

spec :: Spec
spec = do
#if MIN_VERSION_text(2,0,0)
    let it' = it
#else
    let it' t = before_ (pendingWith "Incompatible text version") . it t
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
        let cs' = T.concatMap (\c -> T.pack [c, 'a', ' ', 'a', c, ' ']) cs
        it' "Compare with `text`" do
            C.toTitleFusion cs' `shouldBe` T.toTitle cs'
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


