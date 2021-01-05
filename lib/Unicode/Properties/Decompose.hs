-- |
-- Module      : Unicode.Properties.Decompose
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Unicode.Properties.Decompose
    ( decompose
    , DecomposeMode(..)
    , isDecomposable
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

import Unicode.Properties.Hangul

-------------------------------------------------------------------------------
-- Non Hangul decomposition
-------------------------------------------------------------------------------

data DecomposeMode = Canonical | Kompat

-- | Given the 'DecomposeMode' @D@ and a character @c@, decompose @c@ into its
-- normal form in @D@.
{-# INLINE decompose #-}
decompose :: DecomposeMode -> Char -> [Char]
decompose Canonical  = D.decompose
decompose Kompat = K.decompose

-- | Given the 'DecomposeMode' @D@ and a character @c@, return True if @c@ is
-- decomposable in @D@. This does not work for Hangul characters.
{-# INLINE isDecomposable #-}
isDecomposable :: DecomposeMode -> Char -> Bool
isDecomposable Canonical  = D.isDecomposable
isDecomposable Kompat = K.isDecomposable

-------------------------------------------------------------------------------
-- Hangul decomposition
-------------------------------------------------------------------------------

-- | Decompose a hangul syllable into its corresponding jamo characters.
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
