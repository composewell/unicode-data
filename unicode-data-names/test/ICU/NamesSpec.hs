{-# LANGUAGE BlockArguments #-}

module ICU.NamesSpec
    ( spec
    ) where

import Data.Foldable (traverse_)
import Data.Version (showVersion)
import Test.Hspec
    ( before_
    , describe
    , expectationFailure
    , it
    , pendingWith
    , Spec
    , Expectation
    , HasCallStack )
import qualified Unicode.Char as U
import qualified Unicode.Char.General.Names as String
import qualified ICU.Char as ICU
import qualified ICU.Names as ICUString

spec :: Spec
spec = do
    describe' "name" do
        it "String" do
            traverse_ (check String.name ICUString.name) [minBound..maxBound]
    describe' "correctedName" do
        it "String" do
            traverse_
                (check String.correctedName ICUString.correctedName)
                [minBound..maxBound]
    where
    describe' = if U.unicodeVersion == ICU.unicodeVersion
        then describe
        else \t -> before_ (pendingWith $ mconcat
                    [ "Incompatible ICU Unicode version: expected "
                    , showVersion U.unicodeVersion
                    , ", got: "
                    , showVersion ICU.unicodeVersion ])
                 . describe t
    check
        :: forall a. (HasCallStack, Eq a, Show a)
        => (Char -> Maybe a)
        -> (Char -> Maybe a)
        -> Char
        -> Expectation
    check f fRef c = if n == nRef
        then pure ()
        else expectationFailure $ mconcat
            [ show c
            , ": expected ", maybe "\"\"" show nRef
            , ", got ", maybe "\"\"" show n, "" ]
        where
        !n    = f c
        !nRef = fRef c
