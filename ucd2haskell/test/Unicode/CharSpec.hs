{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Unicode.CharSpec
  ( spec
  ) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List.Split (splitOn)
import Data.Version (showVersion)
import Numeric (showHex)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified System.IO as IO
import Test.Hspec
import Test.HUnit (assertEqual)

import Paths_ucd2haskell (getDataDir)
import qualified Unicode.Char as UChar

spec :: Spec
spec = do
    dataDir <- runIO getDataDir
    let pythonRefFile = dataDir </> "python.csv"
    hasPythonRefFile <- runIO (doesFileExist pythonRefFile)
    when (not hasPythonRefFile) do
        error ("Cannot find file “" <> pythonRefFile <> "”. Run python script first.")
    describe "unicode-data" do
        describe "Compare to Python" do
            -- Read file
            -- content <- runIO (lines <$> readFile' pythonRefFile)
            (!pythonUnicodeVersion, !content) <- runIO $
                IO.withFile pythonRefFile IO.ReadMode \h -> do
                    -- First line is Python’ Unicode version
                    pythonUnicodeVersion <- IO.hGetLine h
                    content <- parse h
                    pure (pythonUnicodeVersion, content)

            -- -- Check Unicode version
            -- let pythonUnicodeVersion = head content
            it "Compatible Python version" do
                pythonUnicodeVersion `shouldBe` showVersion UChar.unicodeVersion
            it "Check characters" do
                -- traverse_ checkChar (parseLine <$> filter (not . null) (tail content))
                traverse_ checkChar content
    where
    checkChar CharEntry{..} = do
        UChar.generalCategory char `shouldBe'` generalCategory
        UChar.toLowerString char `shouldBe'` lower
        UChar.toUpperString char `shouldBe'` upper
        UChar.toTitleString char `shouldBe'` title
        UChar.isLowerCase char `shouldBe'` isLower
        UChar.isUpperCase char `shouldBe'` isUpper
        fmap fromRational (UChar.numericValue char) `shouldBe'` numericValue
        -- [NOTE] Python does not use “DerivedNumericValues.txt”, so it is incomplete
        -- UChar.integerValue char `shouldBe'` integerValue
        where
        shouldBe'
            :: forall a. (HasCallStack, Eq a, Show a)
            => a -> a -> Expectation
        shouldBe' = assertEqual (showCodePoint char "")
    showCodePoint c
        = ("Code point: 0x" <>)
        . fmap UChar.toUpper
        . showHex (UChar.ord c)
    parse h = IO.hIsEOF h >>= \case
        True  -> pure []
        False -> do
            !line <- IO.hGetLine h
            if null line
                then parse h
                else (parseLine line :) <$> parse h
    parseLine line = case splitOn "," line of
        c : gc
          : lower' : upper' : title' : isLower' : isUpper'
          : numericValue' : integerValue'
          : _ -> CharEntry{..}
            where
            char            = parseChar c
            generalCategory = parseGeneralCategory gc
            lower           = parseChar <$> words lower'
            upper           = parseChar <$> words upper'
            title           = parseChar <$> words title'
            isLower         = parseBool isLower'
            isUpper         = parseBool isUpper'
            numericValue    = parseDouble numericValue'
            integerValue    = parseInt integerValue'
        _ -> error ("Cannot parse line: “" <> line <> "”")
    parseChar = UChar.chr . read . ("0x" <>)
    parseBool = read @Bool
    parseGeneralCategory = \case
        "Lu" -> UChar.UppercaseLetter
        "Ll" -> UChar.LowercaseLetter
        "Lt" -> UChar.TitlecaseLetter
        "Lm" -> UChar.ModifierLetter
        "Lo" -> UChar.OtherLetter
        "Mn" -> UChar.NonSpacingMark
        "Mc" -> UChar.SpacingCombiningMark
        "Me" -> UChar.EnclosingMark
        "Nd" -> UChar.DecimalNumber
        "Nl" -> UChar.LetterNumber
        "No" -> UChar.OtherNumber
        "Pc" -> UChar.ConnectorPunctuation
        "Pd" -> UChar.DashPunctuation
        "Ps" -> UChar.OpenPunctuation
        "Pe" -> UChar.ClosePunctuation
        "Pi" -> UChar.InitialQuote
        "Pf" -> UChar.FinalQuote
        "Po" -> UChar.OtherPunctuation
        "Sm" -> UChar.MathSymbol
        "Sc" -> UChar.CurrencySymbol
        "Sk" -> UChar.ModifierSymbol
        "So" -> UChar.OtherSymbol
        "Zs" -> UChar.Space
        "Zl" -> UChar.LineSeparator
        "Zp" -> UChar.ParagraphSeparator
        "Cc" -> UChar.Control
        "Cf" -> UChar.Format
        "Cs" -> UChar.Surrogate
        "Co" -> UChar.PrivateUse
        "Cn" -> UChar.NotAssigned
        cat  -> error ("Unsupported general category: “" <> cat <> "”")
    parseDouble n = if null n then Nothing else Just (read @Double n)
    parseInt    n = if null n then Nothing else Just (read @Int n)

data CharEntry = CharEntry
    { char            :: !Char
    , generalCategory :: !UChar.GeneralCategory
    , lower           :: !String
    , upper           :: !String
    , title           :: !String
    , isLower         :: !Bool
    , isUpper         :: !Bool
    , numericValue    :: !(Maybe Double)
    , integerValue    :: !(Maybe Int) }
