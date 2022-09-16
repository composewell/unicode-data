{-# LANGUAGE BlockArguments, OverloadedLists #-}

module Unicode.Char.Identifiers.SecuritySpec
  ( spec
  ) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Unicode.Char.Identifiers.Security
import Test.Hspec

spec :: Spec
spec = do
    describe "identifierTypes" do
        it "some values" do
            let check ts = (ts ==) . identifierTypes
            minBound  `shouldSatisfy` check [NotCharacter]
            maxBound  `shouldSatisfy` check [NotCharacter]
            '1'       `shouldSatisfy` check [Recommended]
            'a'       `shouldSatisfy` check [Recommended]
            '\x0149'  `shouldSatisfy` check [Deprecated]
            '\x00AD'  `shouldSatisfy` check [DefaultIgnorable]
            '\x1F240' `shouldSatisfy` check [NotNFKC]
            '\x1F4FF' `shouldSatisfy` check [NotXID]
            '\x082C'  `shouldSatisfy` check [Exclusion]
            '\x1680'  `shouldSatisfy` check [Exclusion, NotXID]
            '\x01B9'  `shouldSatisfy` check [Obsolete]
            '\x05C6'  `shouldSatisfy` check [Obsolete, NotXID]
            '\x0234'  `shouldSatisfy` check [Technical]
            '\x2CF1'  `shouldSatisfy` check [Technical, Exclusion]
            '\x20E4'  `shouldSatisfy` check [Technical, NotXID]
            '\x018D'  `shouldSatisfy` check [Technical, Obsolete]
            '\x2E00'  `shouldSatisfy` check [Technical, Obsolete, NotXID]
            '\x018E'  `shouldSatisfy` check [UncommonUse]
            '\x16A40' `shouldSatisfy` check [UncommonUse, Exclusion]
            '\x05A2'  `shouldSatisfy` check [UncommonUse, Obsolete]
            '\xA8FC'  `shouldSatisfy` check [UncommonUse, Obsolete, NotXID]
            '\x025B'  `shouldSatisfy` check [UncommonUse, Technical]
            '\x1D1DE' `shouldSatisfy` check [UncommonUse, Technical, NotXID]
            '\x07FD'  `shouldSatisfy` check [LimitedUse]
            '\xA9CF'  `shouldSatisfy` check [LimitedUse, Exclusion]
            '\x070F'  `shouldSatisfy` check [LimitedUse, NotXID]
            '\x07FA'  `shouldSatisfy` check [LimitedUse, Obsolete]
            '\x1B6B'  `shouldSatisfy` check [LimitedUse, Technical]
            '\x2019'  `shouldSatisfy` check [Inclusion]
            '\x018F'  `shouldSatisfy` check [Recommended]
        it "invariants" do
            let {
                check c = do
                    let ts = identifierTypes c
                    when (NotCharacter     `elem` ts) (ts `shouldBe` [NotCharacter])
                    when (Deprecated       `elem` ts) (ts `shouldBe` [Deprecated])
                    when (DefaultIgnorable `elem` ts) (ts `shouldBe` [DefaultIgnorable])
                    when (NotNFKC          `elem` ts) (ts `shouldBe` [NotNFKC])
                    when (Recommended      `elem` ts) (ts `shouldBe` [Recommended])
                    when (Inclusion        `elem` ts) (ts `shouldBe` [Inclusion])
            }
            -- [TODO] more invariants
            traverse_ check (enumFromTo minBound maxBound)
    it "Identifier status is consistent with identifier types" do
        let checkChar c = all (== identifierStatus c)
                              (identifierTypeStatus <$> identifierTypes c)
        traverse_ (`shouldSatisfy` checkChar) (enumFromTo minBound maxBound)
