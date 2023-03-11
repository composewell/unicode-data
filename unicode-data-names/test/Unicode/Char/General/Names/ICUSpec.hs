{-# LANGUAGE BlockArguments #-}

module Unicode.Char.General.Names.ICUSpec
    ( spec
    ) where

import Data.Foldable (traverse_)
import Data.Maybe ( fromMaybe )
import Data.Version (showVersion)
import Test.Hspec
    ( before_,
      expectationFailure,
      it,
      pendingWith,
      Spec, Expectation, HasCallStack )
import qualified Unicode.Char as U
import qualified Unicode.Char.General.Names as N
import qualified ICU.Names as ICU

spec :: Spec
spec = do
    it' "name" $
        traverse_ (check N.name ICU.name) [minBound..maxBound]
    it' "correctedName" do
        traverse_ (check N.correctedName ICU.correctedName) [minBound..maxBound]
    where
    it' = if U.unicodeVersion /= ICU.unicodeVersion
        then it
        else \t -> before_ (pendingWith $ mconcat
                    [ "Incompatible ICU Unicode version: expected "
                    , showVersion U.unicodeVersion
                    , ", got: "
                    , showVersion ICU.unicodeVersion ])
                 . it t
    check
        :: HasCallStack
        => (Char -> Maybe String)
        -> (Char -> Maybe String)
        -> Char
        -> Expectation
    check f fRef c = if n == nRef
        then pure ()
        else expectationFailure $ mconcat
            [ show c
            , ": expected “", fromMaybe "" nRef
            , "”, got “", fromMaybe "" n, "”" ]
        where
        !n    = f c
        !nRef = fRef c
