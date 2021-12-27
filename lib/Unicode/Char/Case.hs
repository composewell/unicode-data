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
    ( -- * Predicates
      isLowerCase
    , isLower
    , isUpperCase
    , isUpper
    )
where

import qualified Unicode.Internal.Char.DerivedCoreProperties as P

-- | Returns 'True' for lower-case letters.
--
-- @since 0.3.0
{-# INLINE isLowerCase #-}
isLowerCase :: Char -> Bool
isLowerCase = P.isLowercase

-- | Returns 'True' for lower-case letters.
--
-- @since 0.1.0
{-# INLINE isLower #-}
{-# DEPRECATED isLower "Use isLowerCase instead. Note that the behavior of this function does not match base:Data.Char.isLower. See Unicode.Char.Case.Compat for behavior compatible with base:Data.Char." #-}
isLower :: Char -> Bool
isLower = P.isLowercase

-- | Returns 'True' for upper-case letters.
--
-- @since 0.3.0
{-# INLINE isUpperCase #-}
isUpperCase :: Char -> Bool
isUpperCase = P.isUppercase

-- | Returns 'True' for upper-case letters.
--
-- @since 0.1.0
{-# INLINE isUpper #-}
{-# DEPRECATED isUpper "Use isUpperCase instead. Note that the behavior of this function does not match base:Data.Char.isUpper. See Unicode.Char.Case.Compat for behavior compatible with base:Data.Char." #-}
isUpper :: Char -> Bool
isUpper = P.isUppercase
