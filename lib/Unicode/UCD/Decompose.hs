-- |
-- Module      : Unicode.UCD.Decompose
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode normalization.
--
-- For more information please refer to the following sections of the [Unicode
-- standard](https://www.unicode.org/versions/latest/):
--
-- * 2 General Structure
--
--     * 2.3 Compatibility Characters
--     * 2.12 Equivalent Sequences
--
-- * 3 Conformance
--
--     * 3.6 Combination
--     * 3.7 Decomposition
--     * 3.11 Normalization Forms
--     * 3.12 Conjoining Jamo Behavior
--
-- * 4 Character Properties
--
--     * 4.3 Combining Classes
--
-- * [Unicode® Standard Annex #15 - Unicode Normalization Forms](https://www.unicode.org/reports/tr15)
-- * [Unicode® Standard Annex #44 - Unicode Character Database](https://www.unicode.org/reports/tr44/)
--
module Unicode.UCD.Decompose
    ( DecomposeMode(..)
    , isDecomposable
    , decompose
    , decomposeHangul
    )
where

import Control.Exception (assert)
import Data.Char (ord)
import GHC.Base (unsafeChr)
import Unicode.Internal.Division (quotRem21, quotRem28)

import qualified Unicode.Internal.Generated.UnicodeData.Decomposable    as D
import qualified Unicode.Internal.Generated.UnicodeData.DecomposableK   as K
import qualified Unicode.Internal.Generated.UnicodeData.Decompositions  as D
import qualified Unicode.Internal.Generated.UnicodeData.DecompositionsK as K

import Unicode.UCD.Hangul

-------------------------------------------------------------------------------
-- Non Hangul decomposition
-------------------------------------------------------------------------------

-- | Whether we are decomposing in canonical or compatibility mode.
data DecomposeMode = Canonical | Kompat

-- | Decompose a non-Hangul character into its canonical or compatibility
-- decompositions.  Note that the resulting characters may further decompose.
{-# INLINE decompose #-}
decompose :: DecomposeMode -> Char -> [Char]
decompose Canonical  = D.decompose
decompose Kompat = K.decompose

-- | Given a non-Hangul character determine if the character is decomposable.
-- Note that in case compatibility decompositions a character may decompose
-- into a single compatibility character.
{-# INLINE isDecomposable #-}
isDecomposable :: DecomposeMode -> Char -> Bool
isDecomposable Canonical  = D.isDecomposable
isDecomposable Kompat = K.isDecomposable

-------------------------------------------------------------------------------
-- Hangul decomposition
-------------------------------------------------------------------------------

-- | Decompose a Hangul syllable into its corresponding Jamo characters.
{-# INLINE decomposeHangul #-}
decomposeHangul :: Char -> (Char, Char, Char)
decomposeHangul c = (l, v, t)

    where

    i = ord c - hangulFirst
    !(tn, ti) = assert (jamoTCount == 28) quotRem28 i
    !(li, vi) = assert (jamoVCount == 21) quotRem21 tn
    l = unsafeChr (jamoLFirst + li)
    v = unsafeChr (jamoVFirst + vi)
    t = unsafeChr (jamoTFirst + ti)
