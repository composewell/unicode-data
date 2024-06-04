{-# LANGUAGE CPP, BlockArguments #-}

module ICU.NamesSpec
    ( spec
    ) where

import Control.Applicative (Alternative(..))
import Data.Foldable (traverse_)
import Data.Version (showVersion, versionBranch)
import Numeric (showHex)
import Test.Hspec
    ( describe
    , expectationFailure
    , it
    , pendingWith
    , Spec
    , HasCallStack, SpecWith )

import qualified ICU.Char as ICU
import qualified ICU.Names as ICUString
import qualified Unicode.Char as U
import qualified Unicode.Char.General.Names as String
#ifdef HAS_BYTESTRING
import qualified Data.ByteString.Char8 as B8
import qualified Unicode.Char.General.Names.ByteString as ByteString
#endif
#ifdef HAS_TEXT
import qualified Unicode.Char.General.Names.Text as Text
import qualified ICU.Names.Text as ICUText
#endif

spec :: Spec
spec = do
    describe "name" do
        checkAndGatherErrors "String" String.name ICUString.name
#ifdef HAS_BYTESTRING
        checkAndGatherErrors "ByteString"
            ByteString.name
            (fmap B8.pack . ICUString.name)
#endif
#ifdef HAS_TEXT
        checkAndGatherErrors "Text" Text.name ICUText.name
#endif
    describe "correctedName" do
        checkAndGatherErrors "String"
            String.correctedName
            ICUString.correctedName
#ifdef HAS_BYTESTRING
        checkAndGatherErrors "ByteString"
            ByteString.correctedName
            (fmap B8.pack . ICUString.correctedName)
#endif
#ifdef HAS_TEXT
        checkAndGatherErrors "Text"
            Text.correctedName
            ICUText.correctedName
#endif
    where
    ourUnicodeVersion = versionBranch U.unicodeVersion
    theirUnicodeVersion = versionBranch ICU.unicodeVersion
    showCodePoint c = ("U+" ++) . fmap U.toUpper . showHex (U.ord c)

    -- There is no feature to display warnings other than `trace`, so
    -- hack our own:
    -- 1. Compare given functions in pure code and gather warning & errors
    -- 2. Create dummy spec that throw an expectation failure, if relevant.
    -- 3. Create pending spec for each Char that raises a Unicode version
    --    mismatch between ICU and unicode-data.
    checkAndGatherErrors
        :: forall a. (HasCallStack, Eq a, Show a)
        => String
        -> (Char -> Maybe a)
        -> (Char -> Maybe a)
        -> SpecWith ()
    checkAndGatherErrors label f fRef = do
        it label (maybe (pure ()) expectationFailure err)
        if null ws
            then pure ()
            else describe (label ++ " (Unicode version conflict)")
                          (traverse_ mkWarning ws)
        where
        Acc ws err = foldr check (Acc [] Nothing) [minBound..maxBound]
        check c acc
            -- Test passed
            | n == nRef = acc
            -- Unicode version mismatch: char is not mapped in one of the libs:
            -- add warning.
            | age' > ourUnicodeVersion || age' > theirUnicodeVersion
            = acc{warnings=c : warnings acc}
            -- Error
            | otherwise =
                let !msg = mconcat
                        [ showCodePoint c ": expected "
                        , maybe "\"\"" show nRef
                        , ", got ", maybe "\"\"" show n, "" ]
                in acc{firstError = firstError acc <|> Just msg}
            where
            !n    = f c
            !nRef = fRef c
            age = ICU.charAge c
            age' = take 3 (versionBranch age)
        mkWarning c = it (showCodePoint c "") . pendingWith $ mconcat
            [ "Incompatible ICU Unicode version: expected "
            , showVersion U.unicodeVersion
            , ", got: "
            , showVersion ICU.unicodeVersion
            , " (ICU character age is: "
            , showVersion (ICU.charAge c)
            , ")" ]

data Acc = Acc { warnings :: ![Char], firstError :: !(Maybe String) }
