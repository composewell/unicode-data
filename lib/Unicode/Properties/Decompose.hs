-- |
-- Module      : Unicode.Properties.Decompose
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Unicode.Properties.Decompose
    ( decomposeChar
    , DecomposeMode(..)
    , isDecomposable
    )
where

import qualified Unicode.Internal.Properties.Decomposable    as D
import qualified Unicode.Internal.Properties.DecomposableK   as K
import qualified Unicode.Internal.Properties.Decompositions  as D
import qualified Unicode.Internal.Properties.DecompositionsK as K

data DecomposeMode = Canonical | Kompat

-- | Given the 'DecomposeMode' @D@ and a character @c@, decompose @c@ into its
-- normal form in @D@.
{-# INLINE decomposeChar #-}
decomposeChar :: DecomposeMode -> Char -> [Char]
decomposeChar Canonical  = D.decomposeChar
decomposeChar Kompat = K.decomposeChar

-- | Given the 'DecomposeMode' @D@ and a character @c@, return True if @c@ is
-- decomposable in @D@. This does not work for Hangul characters.
{-# INLINE isDecomposable #-}
isDecomposable :: DecomposeMode -> Char -> Bool
isDecomposable Canonical  = D.isDecomposable
isDecomposable Kompat = K.isDecomposable
