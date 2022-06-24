{-# LANGUAGE CPP #-}

-- [TODO] @since
-- |
-- Module      : Unicode.Char.General
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode scripts related functions.
--

module Unicode.Char.General.Scripts
    ( S.Script(..)
    , script
    , scriptDefinition
    , inScript
    )
where

import Data.Char (chr)
import GHC.Exts
       (Ptr(..), Char(..), Int(..),
        indexWord32OffAddr#, word2Int#, int2Word#,
        and#, isTrue#, eqWord#, leWord#, neWord#,
        andI#, (-#), (<#),
        chr#, ord#)
#if MIN_VERSION_base(4,16,0)
import GHC.Exts (word32ToWord#)
#endif
#ifdef WORDS_BIGENDIAN
import GHC.Exts (byteSwap32#)
#endif

import qualified Unicode.Internal.Char.Scripts as S

-- [TODO] @since
-- | Character script
{-# INLINE script #-}
script :: Char -> S.Script
script = toEnum . S.script

{- HLINT ignore scriptDefinition "Eta reduce" -}
-- [TODO] @since
-- | Characters correspinding to a 'S.Script'.
scriptDefinition :: S.Script -> String
scriptDefinition = unpack . S.scriptDefinition
    where
    -- [NOTE] Encoding:
    -- • A single char is encoded as an LE Word32.
    -- • A range is encoded as two LE Word32 (first is lower bound, second is
    --   upper bound), which correspond to the codepoints with the 32th bit set.

    scriptRangeMask# = 0x80000000## -- 1 << 31
    maskComplement#  = 0x7fffffff## -- 1 << 31 ^ 0xffffffff

    unpack (Ptr addr#, I# n#) = let {
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
        getCodePoint k# = word2Int# (and# maskComplement# k#);
        addRange k# acc = if isTrue# (k# <# 0#)
            then acc
            else let {
                r1# = getRawCodePoint k#;
                c1# = getCodePoint r1#;
                isRange = isTrue# (and# r1# scriptRangeMask# `neWord#` 0##)
            } in if isRange
                then let {
                    c2# = getCodePoint (getRawCodePoint (k# -# 1#));
                    acc' = foldr ((:) . chr) acc [I# c2# .. I# c1#]
                } in addRange (k# -# 2#) acc'
                else addRange (k# -# 1#) (C# (chr# c1#) : acc)
    } in addRange (n# -# 1#) mempty

{- HLINT ignore inScript "Eta reduce" -}
-- [TODO] @since
-- | Check if a character is in a 'S.Script'.
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
