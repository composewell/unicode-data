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
            let cs' = C.toLowerStream cs in cs' `shouldBe` C.toLowerStream cs'
#if MIN_VERSION_text(2,0,0)
            let cs' = C.toLowerText cs in cs' `shouldBe` C.toLowerText cs'
#endif
        it' "Compare with `text`" do
            C.toLowerStream cs `shouldBe` T.toLower cs
#if MIN_VERSION_text(2,0,0)
            C.toLowerText cs `shouldBe` T.toLower cs
#endif
    describe "toUpper" do
        it "Idempotent" do
            let cs' = C.toUpperStream cs in cs' `shouldBe` C.toUpperStream cs'
#if MIN_VERSION_text(2,0,0)
            let cs' = C.toUpperText cs in cs' `shouldBe` C.toUpperText cs'
#endif
        it' "Compare with `text`" do
            C.toUpperStream cs `shouldBe` T.toUpper cs
#if MIN_VERSION_text(2,0,0)
            C.toUpperText cs `shouldBe` T.toUpper cs
#endif
    describe "toTitle" do
        let cs' = T.concatMap (\c -> T.pack [c, 'a', ' ', 'a', c, ' ']) cs
        it' "Compare with `text`" do
            C.toTitleStream cs' `shouldBe` T.toTitle cs'
    describe "toCaseFold" do
        it "Idempotent" do
            let cs' = C.toCaseFoldStream cs in cs' `shouldBe` C.toCaseFoldStream cs'
#if MIN_VERSION_text(2,0,0)
            let cs' = C.toCaseFoldText cs in cs' `shouldBe` C.toCaseFoldText cs'
#endif
        it' "Compare with `text`" do
            C.toCaseFoldStream cs `shouldBe` T.toCaseFold cs
#if MIN_VERSION_text(2,0,0)
            C.toCaseFoldText cs `shouldBe` T.toCaseFold cs
#endif


