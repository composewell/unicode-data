{-# LANGUAGE BlockArguments, CPP #-}

module Unicode.Char.General.ScriptsSpec
  ( spec
  ) where

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
import GHC.Exts (byteSwap32#)
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
| 9.0.1       | 4.15.0         | 12.1            |
| 9.2.1       | 4.16.0         | 14.0            |
+-------------+----------------+-----------------+
-}

spec :: Spec
spec = do
  describe "Unicode scripts" do
    it "Characters are in the definition of their corresponding script"
        let {
            check c =
                let s = UScripts.script c
                in if s `inScript` c
                    then pure ()
                    else expectationFailure $ mconcat
                        [ "Char “", show c, "” in not in the definition of “"
                        , show s, "”." ]
        } in traverse_ check [minBound..maxBound]
    it "Characters in a script definition have the corresponding script"
        let {
            checkChar s c = let s' = UScripts.script c in if s' == s
                then pure ()
                else expectationFailure $ mconcat
                    [ "Script is different for “", show c, "”. Expected: “"
                    , show s, "” but got: “", show s', "”." ];
            check s = let chars = UScripts.scriptDefinition s
                      in traverse_ (checkChar s) chars
        } in traverse_ check [minBound..maxBound]

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
            byteSwap32# (word32ToWord# (indexWord32OffAddr# addr# k#));
#else
            byteSwap32# (indexWord32OffAddr# addr# k#);
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
