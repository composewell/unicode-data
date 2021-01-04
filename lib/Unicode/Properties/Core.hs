-- |
-- Module      : Unicode.Properties.Core
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Unicode.Properties.Core
    ( isAlpha
    , isUpper
    , isLower
    , isSpace
    )
where

import qualified Unicode.Internal.Generated.DerivedCoreProperties as P
import qualified Unicode.Internal.Generated.PropList as P

-- | Selects Alphabetic Unicode characters defined in Unicode character
-- database.  This should be equivalent to "Data.Char.isAlpha".
{-# INLINE isAlpha #-}
isAlpha :: Char -> Bool
isAlpha = P.isAlphabetic

-- | Selects Uppercase Unicode characters defined in Unicode character database.
-- This should be equivalent to "Data.Char.isUpper".
{-# INLINE isUpper #-}
isUpper :: Char -> Bool
isUpper = P.isUppercase

-- | Selects Lowercase Unicode characters defined in Unicode character database.
-- This should be equivalent to "Data.Char.isLower".
{-# INLINE isLower #-}
isLower :: Char -> Bool
isLower = P.isLowercase

-- | Selects White_Space Unicode characters defined in Unicode character
-- database.  This should be equivalent to "Data.Char.isSpace".
{-# INLINE isSpace #-}
isSpace :: Char -> Bool
isSpace = P.isWhite_Space
