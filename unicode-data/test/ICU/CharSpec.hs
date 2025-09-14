{-# LANGUAGE CPP, BlockArguments, GADTs #-}

module ICU.CharSpec
    ( spec
    ) where

import Control.Applicative (Alternative(..))
import Data.Bits (Bits(..))
import qualified Data.Char as Char
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
import qualified Unicode.Char as U
import qualified Unicode.Char.Case as C
import qualified Unicode.Char.Case.Compat as CC

spec :: Spec
spec = do
    describe "General" do
        checkAndGatherErrors
            "charType"
            (GeneralCategory . U.generalCategory)
            (GeneralCategory . ICU.toGeneralCategory . ICU.charType)
        checkAndGatherErrors
            "isNoncharacter"
            (GeneralCategory . U.isNoncharacter)
            (GeneralCategory . ICU.isNoncharacter)
    describe "Case" do
        checkAndGatherErrors
            "isLowerCase"
            C.isLowerCase
            ICU.isLowerCase
        checkAndGatherErrors
            "isUpperCase"
            C.isUpperCase
            ICU.isUpperCase
        checkAndGatherErrors
            "isLower"
            CC.isLower
            ICU.isLower
        checkAndGatherErrors
            "isUpper"
            CC.isUpper
            (\c -> ICU.isUpper c || ICU.isTitle c)
        checkAndGatherErrors
            "toLower"
            CC.toLower
            ICU.toLowerCase
        checkAndGatherErrors
            "toUpper"
            CC.toUpper
            ICU.toUpperCase
    -- TODO: other functions
    where
    ourUnicodeVersion = versionBranch U.unicodeVersion
    theirUnicodeVersion = versionBranch ICU.unicodeVersion
    showCodePoint c = ("U+" ++) . fmap U.toUpper . showHex (U.ord c)
    -- Check if the character is not assigned in exactly one Unicode version.
    isUnassigned c = (U.generalCategory c == U.NotAssigned)
               `xor` (ICU.toGeneralCategory (ICU.charType c) == Char.NotAssigned)
    -- Check if the character has changed its general category
    hasDifferentCategory c = fromEnum (U.generalCategory c)
                          /= fromEnum (ICU.toGeneralCategory (ICU.charType c))

    -- There is no feature to display warnings other than `trace`, so
    -- hack our own:
    -- 1. Compare given functions in pure code and gather warning & errors
    -- 2. Create dummy spec that throw an expectation failure, if relevant.
    -- 3. Create pending spec for each Char that raises a Unicode version
    --    mismatch between ICU and unicode-data.
    checkAndGatherErrors
        :: forall a. (HasCallStack, Eq a, Show a)
        => String
        -> (Char -> a)
        -> (Char -> a)
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
            | age' > ourUnicodeVersion || age' > theirUnicodeVersion ||
              isUnassigned c
            = acc{warnings=(c, Unassigned) : warnings acc}
            | hasDifferentCategory c
            = acc{warnings=(c, CategoryChange) : warnings acc}
            -- Error
            | otherwise =
                let !msg = mconcat
                        [ showCodePoint c ": expected "
                        , show nRef
                        , ", got ", show n, "" ]
                in acc{firstError = firstError acc <|> Just msg}
            where
            !n    = f c
            !nRef = fRef c
            age = ICU.charAge c
            age' = take 3 (versionBranch age)
        mkWarning (c, reason) = it (showCodePoint c "") . pendingWith $ mconcat
            [ "Incompatible ICU Unicode version: expected "
            , showVersion U.unicodeVersion
            , ", got: "
            , showVersion ICU.unicodeVersion
            , case reason of
                Unassigned -> mconcat
                    [ " (ICU character age is: "
                    , showVersion (ICU.charAge c)
                    , ")" ]
                CategoryChange -> " (different general category)"
            ]

-- | Helper to compare our GeneralCategory to 'Data.Char.GeneralCategory'.
data GeneralCategory = forall c. (Show c, Enum c) => GeneralCategory c

instance Show GeneralCategory where
    show (GeneralCategory a) = show a

instance Eq GeneralCategory where
    GeneralCategory a == GeneralCategory b = fromEnum a == fromEnum b

data MismatchReason
    = Unassigned
    | CategoryChange

-- | Warning accumulator
data Acc = Acc
    { warnings :: ![(Char, MismatchReason)]
    , firstError :: !(Maybe String) }

