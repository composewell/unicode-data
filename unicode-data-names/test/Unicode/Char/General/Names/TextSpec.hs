{-# LANGUAGE CPP, BlockArguments #-}

module Unicode.Char.General.Names.TextSpec
  ( spec
  ) where

import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Unicode.Char.General.Names as String
import qualified Unicode.Char.General.Names.Text as Text
import Test.Hspec ( Spec, it, shouldBe, Expectation )

spec :: Spec
spec = do
    it "name" do
        traverse_ ((fmap T.unpack . Text.name) `shouldBeEqualTo` String.name)
            [minBound..maxBound]
    it "nameOrAlias" do
        traverse_
            ((fmap T.unpack . Text.nameOrAlias)
                `shouldBeEqualTo` String.nameOrAlias)
            [minBound..maxBound]
    it "correctedName" do
        traverse_
            ((fmap T.unpack . Text.correctedName)
                `shouldBeEqualTo` String.correctedName)
            [minBound..maxBound]
    it "nameAliases" do
        traverse_
            ((fmap T.unpack . Text.nameAliases)
                `shouldBeEqualTo` String.nameAliases)
            [minBound..maxBound]
    it "nameAliasesByType" do
        let check ty = traverse_
                        ((fmap T.unpack . Text.nameAliasesByType ty)
                            `shouldBeEqualTo` String.nameAliasesByType ty)
                        [minBound..maxBound]
        traverse_ check [minBound..maxBound]
    it "nameAliasesWithTypes" do
        traverse_
            ((fmap (fmap (fmap T.unpack)) . Text.nameAliasesWithTypes)
                `shouldBeEqualTo` String.nameAliasesWithTypes)
            [minBound..maxBound]
    where
    shouldBeEqualTo
        :: forall a. (Eq a, Show a)
        => (Char -> a)
        -> (Char -> a)
        -> Char
        -> Expectation
    shouldBeEqualTo f fRef c = f c `shouldBe` fRef c
