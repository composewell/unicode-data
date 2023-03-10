{-# LANGUAGE BlockArguments #-}

module Unicode.Char.General.Scripts.ICUSpec
    ( spec
    ) where

import Data.Foldable (traverse_)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Test.Hspec ( Spec, it, expectationFailure, shouldSatisfy )
import qualified Unicode.Char.General.Scripts as S
import qualified ICU.Scripts as ICU

spec :: Spec
spec = do
    let icuScripts = (\s -> (ICU.scriptShortName s, s)) <$> [minBound..maxBound]
    it "scriptShortName"
        let check = isJust . (`lookup` icuScripts) . S.scriptShortName
        in traverse_ (`shouldSatisfy` check) [minBound..maxBound]
    it "script"
        let check c = if s == sRef
                then pure ()
                else expectationFailure $ mconcat
                    [ show c, ": expected “", sRef, "”, got “", s, "”" ]
                where
                !s    = S.scriptShortName (S.script c)
                !sRef = ICU.scriptShortName (ICU.script c)
        in traverse_ check [minBound..maxBound]
    it "scriptDefinition"
        let check s = case lookup (S.scriptShortName s) icuScripts of
                        Nothing -> error "Cannot convert script"
                        Just s' ->
                            let !defRef = filter ((== s') . ICU.script) [minBound..maxBound]
                                !def    = S.scriptDefinition s
                            in def == defRef
        in traverse_ (`shouldSatisfy` check) [minBound..maxBound]
    it "scriptExtensions"
        let check c = if es == esRef
                then pure ()
                else expectationFailure $ mconcat
                    [ show c
                    , ": expected “", show esRef
                    , "”, got “", show es, "”" ]
                where
                !es = sort (S.scriptShortName <$> NE.toList (S.scriptExtensions c))
                !esRef = sort (ICU.scriptShortName <$> ICU.scriptExtensions c)
        in traverse_ check [minBound..maxBound]
