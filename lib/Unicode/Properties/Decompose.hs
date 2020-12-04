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
    )
where

import qualified Unicode.Internal.Generated.UnicodeData.Decomposable    as D
import qualified Unicode.Internal.Generated.UnicodeData.DecomposableK   as K
import qualified Unicode.Internal.Generated.UnicodeData.Decompositions  as D
import qualified Unicode.Internal.Generated.UnicodeData.DecompositionsK as K

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
