{-# LANGUAGE BlockArguments, OverloadedLists #-}

module Unicode.Char.Identifiers.SecuritySpec
  ( spec
  ) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Unicode.Char.Identifiers.Security
import Test.Hspec

spec :: Spec
spec = do
    describe "identifierTypes" do
        it "some values" do
            let shouldBe' :: (HasCallStack) => Char -> NE.NonEmpty IdentifierType -> Expectation
                shouldBe' c = shouldBe (identifierTypes c)
            minBound  `shouldBe'` [NotCharacter]
            maxBound  `shouldBe'` [NotCharacter]
            '1'       `shouldBe'` [Recommended]
            'a'       `shouldBe'` [Recommended]
            '\x0149'  `shouldBe'` [Deprecated]
            '\x00AD'  `shouldBe'` [DefaultIgnorable]
            '\x1F240' `shouldBe'` [NotNFKC]
            '\x1F4FF' `shouldBe'` [NotXID]
            '\x082C'  `shouldBe'` [Exclusion]
            '\x1680'  `shouldBe'` [Exclusion, NotXID]
            '\x01B9'  `shouldBe'` [Obsolete]
            '\x05C6'  `shouldBe'` [Obsolete, NotXID]
            '\x0234'  `shouldBe'` [Technical]
            '\x2CF1'  `shouldBe'` [Technical, Exclusion]
            '\x20E4'  `shouldBe'` [Technical, NotXID]
            '\x018D'  `shouldBe'` [Technical, Obsolete]
            '\x2E00'  `shouldBe'` [Technical, Obsolete, NotXID]
            '\x018E'  `shouldBe'` [UncommonUse]
            '\x16A40' `shouldBe'` [UncommonUse, Exclusion]
            '\x05A2'  `shouldBe'` [UncommonUse, Obsolete]
            '\xA8FC'  `shouldBe'` [UncommonUse, Obsolete, NotXID]
            '\x025B'  `shouldBe'` [UncommonUse, Technical]
            '\x1D1DE' `shouldBe'` [UncommonUse, Technical, NotXID]
            '\x07FD'  `shouldBe'` [LimitedUse]
            '\xA9CF'  `shouldBe'` [LimitedUse, UncommonUse]
            '\x070F'  `shouldBe'` [LimitedUse, NotXID]
            '\x07FA'  `shouldBe'` [LimitedUse, Obsolete]
            '\x1B6B'  `shouldBe'` [LimitedUse, Technical]
            '\x2019'  `shouldBe'` [Inclusion]
            '\x018F'  `shouldBe'` [Recommended]
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
        let checkChar c = all ((== isAllowedInIdentifier c) . isIdentifierTypeAllowed)
                              (identifierTypes c)
        traverse_ (`shouldSatisfy` checkChar) (enumFromTo minBound maxBound)
