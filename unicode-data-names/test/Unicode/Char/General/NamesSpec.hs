{-# LANGUAGE BlockArguments #-}

module Unicode.Char.General.NamesSpec
  ( spec
  ) where

import Unicode.Char.General
import Unicode.Char.General.Names
import Data.Foldable (traverse_)
import Test.Hspec

spec :: Spec
spec = do
    it "name: Test some characters" do
        name minBound  `shouldBe` Nothing
        name 'A'       `shouldBe` Just "LATIN CAPITAL LETTER A"
        name '\x0080'  `shouldBe` Nothing
        name '\x009f'  `shouldBe` Nothing
        -- Name with correction
        name '\x01A2'  `shouldBe` Just "LATIN CAPITAL LETTER OI"
        name '\x2200'  `shouldBe` Just "FOR ALL"
        -- Name generated using pattern (example from UCD file)
        name '\x4e12'  `shouldBe` Just "CJK UNIFIED IDEOGRAPH-4E12"
        name '\xfeff'  `shouldBe` Just "ZERO WIDTH NO-BREAK SPACE"
        name '\xffff'  `shouldBe` Nothing
        -- Name with correction
        name '\x1D0C5' `shouldBe` Just "BYZANTINE MUSICAL SYMBOL FHTORA SKLIRON CHROMA VASIS"
        name '\x1f41d' `shouldBe` Just "HONEYBEE"
        -- Name generated using pattern (example from UCD file)
        name '\x2f89f' `shouldBe` Just "CJK COMPATIBILITY IDEOGRAPH-2F89F"
        -- Last name defined, as of Unicode 15.0.0
        name '\xe01ef' `shouldBe` Just "VARIATION SELECTOR-256"
        name maxBound  `shouldBe` Nothing
    it "correctedName: Test some characters" do
        correctedName minBound  `shouldBe` Nothing
        correctedName 'A'       `shouldBe` Just "LATIN CAPITAL LETTER A"
        correctedName '\x0080'  `shouldBe` Nothing
        correctedName '\x009f'  `shouldBe` Nothing
        -- Name with correction
        correctedName '\x01A2'  `shouldBe` Just "LATIN CAPITAL LETTER GHA"
        correctedName '\x2200'  `shouldBe` Just "FOR ALL"
        -- Name generated using pattern (example from UCD file)
        correctedName '\x4e12'  `shouldBe` Just "CJK UNIFIED IDEOGRAPH-4E12"
        correctedName '\xfeff'  `shouldBe` Just "ZERO WIDTH NO-BREAK SPACE"
        correctedName '\xffff'  `shouldBe` Nothing
        -- Name with correction
        correctedName '\x1D0C5' `shouldBe` Just "BYZANTINE MUSICAL SYMBOL FTHORA SKLIRON CHROMA VASIS"
        correctedName '\x1f41d' `shouldBe` Just "HONEYBEE"
        -- Name generated using pattern (example from UCD file)
        correctedName '\x2f89f' `shouldBe` Just "CJK COMPATIBILITY IDEOGRAPH-2F89F"
        -- Last name defined, as of Unicode 15.0.0
        correctedName '\xe01ef' `shouldBe` Just "VARIATION SELECTOR-256"
        correctedName maxBound  `shouldBe` Nothing
    it "nameOrAlias: Test some characters" do
        nameOrAlias minBound  `shouldBe` Just "NULL"
        nameOrAlias 'A'       `shouldBe` Just "LATIN CAPITAL LETTER A"
        nameOrAlias '\x0080'  `shouldBe` Just "PADDING CHARACTER"
        nameOrAlias '\x009f'  `shouldBe` Just "APPLICATION PROGRAM COMMAND"
        -- Name with correction
        nameOrAlias '\x01A2'  `shouldBe` Just "LATIN CAPITAL LETTER OI"
        nameOrAlias '\x2200'  `shouldBe` Just "FOR ALL"
        -- Name generated using pattern (example from UCD file)
        nameOrAlias '\x4e12'  `shouldBe` Just "CJK UNIFIED IDEOGRAPH-4E12"
        nameOrAlias '\xfeff'  `shouldBe` Just "ZERO WIDTH NO-BREAK SPACE"
        nameOrAlias '\xffff'  `shouldBe` Nothing
        -- Name with correction
        nameOrAlias '\x1D0C5' `shouldBe` Just "BYZANTINE MUSICAL SYMBOL FHTORA SKLIRON CHROMA VASIS"
        nameOrAlias '\x1f41d' `shouldBe` Just "HONEYBEE"
        -- Name generated using pattern (example from UCD file)
        nameOrAlias '\x2f89f' `shouldBe` Just "CJK COMPATIBILITY IDEOGRAPH-2F89F"
        -- Last name defined, as of Unicode 15.0.0
        nameOrAlias '\xe01ef' `shouldBe` Just "VARIATION SELECTOR-256"
        nameOrAlias maxBound  `shouldBe` Nothing
    it "Every defined character has at least a name or an alias" do
        let checkName c = case nameOrAlias c of
                        Just _  -> True
                        Nothing -> case generalCategory c of
                            Surrogate   -> True
                            PrivateUse  -> True
                            NotAssigned -> True
                            _           -> False
        traverse_ (`shouldSatisfy` checkName) [minBound..maxBound]
    describe "label" do
        it "Some characters" do
            label '\x0000'   `shouldBe` "control-0000"
            label '\x009F'   `shouldBe` "control-009F"
            label 'a'        `shouldBe` "UNDEFINED"
            label '1'        `shouldBe` "UNDEFINED"
            label '\x1D0C5'  `shouldBe` "UNDEFINED"
            label '\x2F89F'  `shouldBe` "UNDEFINED"
            label '\xE000'   `shouldBe` "private-use-E000"
            label '\x10FFFD' `shouldBe` "private-use-10FFFD"
            label '\xD800'   `shouldBe` "surrogate-D800"
            label '\xDFFF'   `shouldBe` "surrogate-DFFF"
            label '\xFDD0'   `shouldBe` "noncharacter-FDD0"
            label '\x10FFFF' `shouldBe` "noncharacter-10FFFF"
            label '\x0378'   `shouldBe` "reserved-0378"
            label '\x1FFFD'  `shouldBe` "reserved-1FFFD"
            label '\xEFFFD'  `shouldBe` "reserved-EFFFD"
    it "Every character has either a name or a label" do
        let checkName c = case name c of
                        Just _  -> True
                        Nothing -> label c /= "UNDEFINED"
        traverse_ (`shouldSatisfy` checkName) [minBound..maxBound]
