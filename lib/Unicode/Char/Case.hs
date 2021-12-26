-- |
-- Module      : Unicode.Char.Case
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Case and case mapping related functions.
--
module Unicode.Char.Case
    ( isLower
    , isUpper
    )
where

import qualified Unicode.Internal.Char.DerivedCoreProperties as P

-- | Returns 'True' for lower-case letters.
--
-- prop> isLower c == Data.Char.isLower c
--
-- @since 0.1.0
{-# INLINE isLower #-}
isLower :: Char -> Bool
isLower = P.isLowercase

-- | Returns 'True' for upper-case or title-case letters.  Title case is used by
-- a small number of letter ligatures like the single-character form of /Lj/.
--
-- prop> isUpper c == Data.Char.isUpper c
--
-- @since 0.1.0
{-# INLINE isUpper #-}
isUpper :: Char -> Bool
isUpper = P.isUppercase
