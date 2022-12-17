{-# LANGUAGE CPP #-}

-- |
-- Module      : Unicode.Char.General.Scripts
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode [scripts](https://www.unicode.org/reports/tr24/) related functions.
--
-- @since 0.1.0
--

module Unicode.Char.General.Scripts
    ( S.Script(..)
    , script
    , scriptExtensions
    , scriptDefinition
    )
where

#include "MachDeps.h"

import Data.Char (chr)
import Data.List.NonEmpty (NonEmpty)
import GHC.Exts
       (Ptr(..), Char(..), Int(..),
        indexWord32OffAddr#, word2Int#,
        and#, isTrue#, neWord#, (-#), (<#), chr#)
#if MIN_VERSION_base(4,16,0)
import GHC.Exts (word32ToWord#)
#endif
#ifdef WORDS_BIGENDIAN
import GHC.Exts (byteSwap32#, narrow32Word#)
#endif

import qualified Unicode.Internal.Char.Scripts as S
import qualified Unicode.Internal.Char.ScriptExtensions as S

-- | Character [script](https://www.unicode.org/reports/tr24/).
--
-- @since 0.1.0
{-# INLINE script #-}
script :: Char -> S.Script
script = toEnum . S.script

{- HLINT ignore scriptDefinition "Eta reduce" -}
-- | Characters corresponding to a 'S.Script'.
--
-- @since 0.1.0
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
            narrow32Word# (byteSwap32# (word32ToWord# (indexWord32OffAddr# addr# k#)));
#else
            narrow32Word# (byteSwap32# (indexWord32OffAddr# addr# k#));
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

-- | Character
-- [script extensions](https://www.unicode.org/reports/tr24/#Script_Extensions).
--
-- @since 0.1.0
{-# INLINE scriptExtensions #-}
scriptExtensions :: Char -> NonEmpty S.Script
scriptExtensions = S.decodeScriptExtensions . S.scriptExtensions
