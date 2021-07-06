-- |
-- Module      : Unicode.Char.Case
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Unicode.Char.Case
    ( isLower
    , isUpper
    )
where

import qualified Unicode.Internal.Generated.DerivedCoreProperties as P

-- | Returns 'True' for lower-case letters.
--
-- prop> isLower == Data.Char.isLower
--
{-# INLINE isLower #-}
isLower :: Char -> Bool
isLower = P.isLowercase

-- | Returns 'True' for upper-case or title-case letters.  Title case is used by
-- a small number of letter ligatures like the single-character form of /Lj/.
--
-- prop> isUpper == Data.Char.isUpper
--
{-# INLINE isUpper #-}
isUpper :: Char -> Bool
isUpper = P.isUppercase
