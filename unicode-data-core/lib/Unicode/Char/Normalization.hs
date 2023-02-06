-- |
-- Module      : Unicode.Char.Normalization
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Low level Unicode database functions to facilitate Unicode normalization.
--
-- For more information on Unicode normalization please refer to the following
-- sections of the [Unicode standard](https://www.unicode.org/versions/latest/):
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

module Unicode.Char.Normalization
    (
    -- * Combining class
      isCombining
    , combiningClass
    , isCombiningStarter

    -- * Composition
    , compose
    , composeStarters

    -- * Decomposition
    -- ** Non-Hangul
    , DecomposeMode(..)
    , isDecomposable
    , decompose

    -- ** Hangul
    , decomposeHangul
    )
where

import Control.Exception (assert)
import Data.Char (ord)
import GHC.Base (unsafeChr)
import Unicode.Internal.Division (quotRem21, quotRem28)
import Unicode.Char.General
    (hangulFirst, jamoLFirst, jamoTCount, jamoTFirst, jamoVCount, jamoVFirst)

import qualified Unicode.Internal.Char.UnicodeData.CombiningClass  as CC
import qualified Unicode.Internal.Char.UnicodeData.Compositions    as C
import qualified Unicode.Internal.Char.UnicodeData.Decomposable    as D
import qualified Unicode.Internal.Char.UnicodeData.DecomposableK   as K
import qualified Unicode.Internal.Char.UnicodeData.Decompositions  as D
import qualified Unicode.Internal.Char.UnicodeData.DecompositionsK as K

-------------------------------------------------------------------------------
-- Compose
-------------------------------------------------------------------------------

-- | Compose a starter character (combining class 0) with a combining character
-- (non-zero combining class). Returns the composed character if the starter
-- combines with the combining character, returns 'Nothing' otherwise.
--
-- @since 0.1.0
{-# INLINE compose #-}
compose :: Char -> Char -> Maybe Char
compose = C.compose

-- | Compose a starter character with another starter character.  Returns the
-- composed character if the two starters combine, returns 'Nothing' otherwise.
--
-- @since 0.1.0
{-# INLINE composeStarters #-}
composeStarters :: Char -> Char -> Maybe Char
composeStarters = C.composeStarters

-- | Return 'True' if a starter character may combine with some preceding
-- starter character.
--
-- @since 0.1.0
{-# INLINE isCombiningStarter #-}
isCombiningStarter :: Char -> Bool
isCombiningStarter = C.isSecondStarter

-------------------------------------------------------------------------------
-- Decompose
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Non Hangul decomposition
-------------------------------------------------------------------------------

-- | Whether we are decomposing in canonical or compatibility mode.
--
-- @since 0.1.0
data DecomposeMode = Canonical | Kompat

-- | Decompose a non-Hangul character into its canonical or compatibility
-- decompositions.  Note that the resulting characters may further decompose.
--
-- @since 0.1.0
{-# INLINE decompose #-}
decompose :: DecomposeMode -> Char -> [Char]
decompose Canonical  = D.decompose
decompose Kompat = K.decompose

-- | Given a non-Hangul character determine if the character is decomposable.
-- Note that in case compatibility decompositions a character may decompose
-- into a single compatibility character.
--
-- @since 0.1.0
{-# INLINE isDecomposable #-}
isDecomposable :: DecomposeMode -> Char -> Bool
isDecomposable Canonical  = D.isDecomposable
isDecomposable Kompat = K.isDecomposable

-------------------------------------------------------------------------------
-- Hangul decomposition
-------------------------------------------------------------------------------

-- | Decompose a Hangul syllable into its corresponding Jamo characters.
--
-- @since 0.1.0
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

-------------------------------------------------------------------------------
-- Combining class
-------------------------------------------------------------------------------

-- Determine the combining properties of characters.

-- | Returns the combining class of a character.
--
-- @since 0.1.0
{-# INLINE combiningClass #-}
combiningClass :: Char -> Int
combiningClass = CC.combiningClass

-- | Returns 'True' if a character is a combining character.
--
-- @since 0.1.0
{-# INLINE isCombining #-}
isCombining :: Char -> Bool
isCombining = CC.isCombining
