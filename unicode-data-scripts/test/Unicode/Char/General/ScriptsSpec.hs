{-# LANGUAGE BlockArguments, CPP, OverloadedLists #-}

module Unicode.Char.General.ScriptsSpec
  ( spec
  ) where

#include "MachDeps.h"

import Data.Foldable (traverse_)
import Test.Hspec
import qualified Unicode.Char.General.Scripts as UScripts
import qualified Unicode.Internal.Char.Scripts as S

import GHC.Exts
       (Ptr(..), Char(..), Int(..),
        indexWord32OffAddr#, int2Word#,
        and#, isTrue#, eqWord#, leWord#, neWord#,
        andI#, (-#), (<#),
        ord#)
#if MIN_VERSION_base(4,16,0)
import GHC.Exts (word32ToWord#)
#endif
#ifdef WORDS_BIGENDIAN
import GHC.Exts (byteSwap32#, narrow32Word#)
#endif

{- [NOTE]
These tests may fail if the compiler’s Unicode version
does not match the version of this package.

+-------------+----------------+-----------------+
| GHC version | @base@ version | Unicode version |
+=============+================+=================+
| 8.8         | 4.13           | 12.0            |
| 8.10.[1-4]  | 4.14.{0,1}     | 12.0            |
| 8.10.5+     | 4.14.2+        | 13.0            |
| 9.0.[1-2]   | 4.15.0         | 12.1            |
| 9.2.[1-6]   | 4.16.0         | 14.0            |
| 9.4.[1-4]   | 4.17.0         | 14.0            |
| 9.6.1       | 4.18.0         | 15.0            |
+-------------+----------------+-----------------+
-}

spec :: Spec
spec = do
  describe "Unicode scripts" do
    describe "Examples" do
        it "script" do
            let check s = (== s) . UScripts.script
            minBound  `shouldSatisfy` check UScripts.Common
            maxBound  `shouldSatisfy` check UScripts.Unknown
            '.'       `shouldSatisfy` check UScripts.Common
            '1'       `shouldSatisfy` check UScripts.Common
            'A'       `shouldSatisfy` check UScripts.Latin
            'Α'       `shouldSatisfy` check UScripts.Greek -- Greek capital
            'α'       `shouldSatisfy` check UScripts.Greek
            '\x0300'  `shouldSatisfy` check UScripts.Inherited
            '\x0485'  `shouldSatisfy` check UScripts.Inherited
            '\x0600'  `shouldSatisfy` check UScripts.Arabic
            '\x060c'  `shouldSatisfy` check UScripts.Common
            '\x0965'  `shouldSatisfy` check UScripts.Common
            '\x1100'  `shouldSatisfy` check UScripts.Hangul
            '\x3000'  `shouldSatisfy` check UScripts.Common
            '\x4E00'  `shouldSatisfy` check UScripts.Han
            '\x11FD0' `shouldSatisfy` check UScripts.Tamil
            '\x1F600' `shouldSatisfy` check UScripts.Common
            '\x20000' `shouldSatisfy` check UScripts.Han
            -- BOM
            '\xFEFF'  `shouldSatisfy` check UScripts.Common
            '\xFFFF'  `shouldSatisfy` check UScripts.Unknown
            -- Private Use Areas
            '\xE000'  `shouldSatisfy` check UScripts.Unknown
            '\xF0000' `shouldSatisfy` check UScripts.Unknown
        it "scriptExtensions" do
            let check s = (== s) . UScripts.scriptExtensions
            minBound  `shouldSatisfy` check [ UScripts.Common]
            maxBound  `shouldSatisfy` check [ UScripts.Unknown]
            '.'       `shouldSatisfy` check [ UScripts.Common]
            '1'       `shouldSatisfy` check [ UScripts.Common]
            'A'       `shouldSatisfy` check [ UScripts.Latin]
            'Α'       `shouldSatisfy` check [ UScripts.Greek]
            'α'       `shouldSatisfy` check [ UScripts.Greek]
            '\x0300'  `shouldSatisfy` check [ UScripts.Inherited]
            '\x0485'  `shouldSatisfy` check [ UScripts.Cyrillic, UScripts.Latin]
            '\x0600'  `shouldSatisfy` check [ UScripts.Arabic]
            '\x060C'  `shouldSatisfy` check [ UScripts.Arabic
                                            , UScripts.Nko
                                            , UScripts.HanifiRohingya
                                            , UScripts.Syriac
                                            , UScripts.Thaana
                                            , UScripts.Yezidi ]
            '\x0965'  `shouldSatisfy` check [ UScripts.Bengali
                                            , UScripts.Devanagari
                                            , UScripts.Dogra
                                            , UScripts.GunjalaGondi
                                            , UScripts.MasaramGondi
                                            , UScripts.Grantha
                                            , UScripts.Gujarati
                                            , UScripts.Gurmukhi
                                            , UScripts.Kannada
                                            , UScripts.Limbu
                                            , UScripts.Mahajani
                                            , UScripts.Malayalam
                                            , UScripts.Nandinagari
                                            , UScripts.Oriya
                                            , UScripts.Khudawadi
                                            , UScripts.Sinhala
                                            , UScripts.SylotiNagri
                                            , UScripts.Takri
                                            , UScripts.Tamil
                                            , UScripts.Telugu
                                            , UScripts.Tirhuta ]
            '\x1100'  `shouldSatisfy` check [ UScripts.Hangul]
            '\x3001'  `shouldSatisfy` check [ UScripts.Bopomofo
                                            , UScripts.Hangul
                                            , UScripts.Han
                                            , UScripts.Hiragana
                                            , UScripts.Katakana
                                            , UScripts.Yi ]
            '\x4E00'  `shouldSatisfy` check [ UScripts.Han]
            '\x11FD0' `shouldSatisfy` check [ UScripts.Grantha, UScripts.Tamil ]
            '\x1F600' `shouldSatisfy` check [ UScripts.Common]
            '\x20000' `shouldSatisfy` check [ UScripts.Han]
            -- BOM
            '\xFEFF'  `shouldSatisfy` check [ UScripts.Common ]
            '\xFFFF'  `shouldSatisfy` check [ UScripts.Unknown ]
            -- Private Use Areas
            '\xE000'  `shouldSatisfy` check [ UScripts.Unknown ]
            '\xF0000' `shouldSatisfy` check [ UScripts.Unknown ]
    it "Characters are in the definition of their corresponding script"
        let {
            check c =
                let s = UScripts.script c
                in if s `inScript` c
                    then pure ()
                    else expectationFailure $ mconcat
                        [ "Char “", show c, "” in not in the definition of “"
                        , show s, "”." ]
        } in traverse_ check (enumFromTo minBound maxBound)
    it "Characters in a script definition have the corresponding script"
        let {
            checkChar s c = let s' = UScripts.script c in if s' == s
                then pure ()
                else expectationFailure $ mconcat
                    [ "Script is different for “", show c, "”. Expected: “"
                    , show s, "” but got: “", show s', "”." ];
            check s = let chars = UScripts.scriptDefinition s
                      in traverse_ (checkChar s) chars
        } in traverse_ check (enumFromTo minBound maxBound)
    it "Characters in with a script extension different from its script"
        let {
            check c =
                let script = UScripts.script c
                    exts = UScripts.scriptExtensions c
                in if  exts == pure script
                    || (isSpecialScript script && script `notElem` exts)
                    || (script `elem` exts)
                    then pure ()
                    else expectationFailure (show (c, script, exts));
            isSpecialScript = \case
                UScripts.Common    -> True
                UScripts.Inherited -> True
                _                  -> False
        } in traverse_ check (enumFromTo minBound maxBound)

{- HLINT ignore inScript "Eta reduce" -}
-- Check if a character is in a 'S.Script'.
-- This is faster than testing the string from UScripts.scriptDefinition
inScript :: S.Script -> Char -> Bool
inScript s (C# c#) = check (S.scriptDefinition s)
    where
    -- [NOTE] see 'scriptDefinition' for the description of the encoding.

    scriptRangeMask# = 0x80000000## -- 1 << 31
    maskComplement#  = 0x7fffffff## -- 1 << 31 ^ 0xffffffff
    cp# = int2Word# (ord# c#)

    check (Ptr addr#, I# n#) = let {
        getRawCodePoint k# =
#ifdef WORDS_BIGENDIAN
#if MIN_VERSION_base(4,16,0)
            narrow32Word# (byteSwap32# (word32ToWord# (indexWord32OffAddr# addr# k#)));
#else
            narrow32Word# (byteSwap32# (indexWord32OffAddr# addr# k#));
#endif
#elif MIN_VERSION_base(4,16,0)
            word32ToWord# (indexWord32OffAddr# addr# k#);
#else
            indexWord32OffAddr# addr# k#;
#endif
        getCodePoint k# = and# maskComplement# k#;
        find k# = not (isTrue# (k# <# 0#)) &&
            let {
                r1# = getRawCodePoint k#;
                c1# = getCodePoint r1#;
                isRange = isTrue# (and# r1# scriptRangeMask# `neWord#` 0##)
            } in if isRange
                then let {
                    c2# = getCodePoint (getRawCodePoint (k# -# 1#));
                    found = isTrue# ((c2# `leWord#` cp#) `andI#` (cp# `leWord#` c1#))
                } in found || find (k# -# 2#)
                else isTrue# (c1# `eqWord#` cp#) || find (k# -# 1#)
    } in find (n# -# 1#)
