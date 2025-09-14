{-# LANGUAGE BlockArguments #-}

module ICU.ScriptsSpec
    ( spec
    ) where

import Data.Char (toUpper, ord)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Version (versionBranch, showVersion)
import Debug.Trace (trace, traceM)
import Numeric (showHex)
import Test.Hspec ( Spec, it, expectationFailure, shouldSatisfy )

import qualified ICU.Char as ICU
import qualified ICU.Scripts as ICU
import qualified Unicode.Char.General.Scripts as S

spec :: Spec
spec = do
    it "scriptShortName"
        let check s = case toIcuScript s of
                    Just _ -> True
                    Nothing
                        | versionMismatch -> trace (mconcat
                            [ "[WARNING] Cannot test scriptShortName for "
                            , show s
                            , ": incompatible ICU version ("
                            , showVersion ICU.unicodeVersion
                            , " /= "
                            , showVersion S.unicodeVersion
                            , ")." ]) True
                        | otherwise -> False
        in traverse_ (`shouldSatisfy` check) [minBound..maxBound]
    it "script"
        let check c
                | s == sRef = pure ()
                | versionMismatch = traceM . mconcat $
                    [ "[WARNING] Cannot test "
                    , showCodePoint c
                    , ": incompatible ICU version ("
                    , showVersion ICU.unicodeVersion
                    , " /= "
                    , showVersion S.unicodeVersion
                    , "). Expected "
                    , show sRef
                    , ", but got: "
                    , show s ]
                | otherwise = expectationFailure $ mconcat
                    [ show c, ": expected “", sRef, "”, got “", s, "”" ]
                where
                !s    = S.scriptShortName (S.script c)
                !sRef = ICU.scriptShortName (ICU.script c)
        in traverse_ check [minBound..maxBound]
    it "scriptDefinition"
        let {
        check s =
            case lookup (S.scriptShortName s) icuScripts of
                Nothing
                    | ourUnicodeVersion > theirUnicodeVersion
                    -> traceM . mconcat $
                        [ "[WARNING] Cannot convert script "
                        , show s
                        , ": incompatible ICU version ("
                        , showVersion ICU.unicodeVersion
                        , " /= "
                        , showVersion S.unicodeVersion
                        , "). "
                        , "Max supported ICU script:"
                        , show ICU.maxSupportedScript ]
                    | otherwise -> error ("Cannot convert script: " ++ show s)
                Just s'
                    | def == defRef -> pure ()
                    | ourUnicodeVersion /= theirUnicodeVersion
                    -> traceM . mconcat $
                        [ "[WARNING] Cannot test "
                        , show s
                        , ": incompatible ICU version ("
                        , showVersion ICU.unicodeVersion
                        , " /= "
                        , showVersion S.unicodeVersion
                        , ")."
                        , if null missing
                            then ""
                            else " Missing: " ++ show missing
                        , "."
                        , if null unexpected
                            then ""
                            else " Unexpected: " ++ show unexpected
                        ]
                    | otherwise -> expectationFailure $ mconcat
                        [ show s
                        , ": expected “", show def
                        , "”, got “", show defRef, "”" ]
                    where
                        !defRef = filter ((== s') . ICU.script) [minBound..maxBound]
                        !def    = S.scriptDefinition s
                        (missing, unexpected) = case s of
                            -- No diff for “Unknown” script, lists are too big
                            S.Unknown -> mempty
                            _ -> (defRef L.\\ def, def L.\\ defRef)
        } in traverse_ check [minBound..maxBound]
    it "scriptExtensions"
        let check c
                | es == esRef = pure ()
                | versionMismatch = traceM . mconcat $
                    [ "[WARNING] Cannot test "
                    , showCodePoint c
                    , ": incompatible ICU version ("
                    , showVersion ICU.unicodeVersion
                    , " /= "
                    , showVersion S.unicodeVersion
                    , "). Expected "
                    , show esRef
                    , ", but got: "
                    , show es ]
                | otherwise = expectationFailure $ mconcat
                    [ show c
                    , ": expected “", show esRef
                    , "”, got “", show es, "”" ]
                where
                !es = NE.sort (S.scriptShortName <$> S.scriptExtensions c)
                !esRef = NE.sort (ICU.scriptShortName <$> ICU.scriptExtensions c)
        in traverse_ check [minBound..maxBound]
    where
    ourUnicodeVersion = versionBranch S.unicodeVersion
    theirUnicodeVersion = take 3 (versionBranch ICU.unicodeVersion)
    showCodePoint c = ("U+" ++) . fmap toUpper $ showHex (ord c) ""
    versionMismatch = ourUnicodeVersion /= theirUnicodeVersion
    icuScripts = (\s -> (ICU.scriptShortName s, s)) <$> [minBound..maxBound]
    toIcuScript = (`lookup` icuScripts) . S.scriptShortName
