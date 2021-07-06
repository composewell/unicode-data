-- |
-- Module      : Unicode.UCD.Core
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Unicode.UCD.Core
    ( isAlpha
    , isUpper
    , isLower
    , isSpace
    )
where

import qualified Unicode.Internal.Generated.DerivedCoreProperties as P
import qualified Unicode.Internal.Generated.PropList as P

-- | Returns 'True' for alphabetic Unicode characters (lower-case, upper-case
-- and title-case letters, plus letters of caseless scripts and modifiers
-- letters).
--
-- prop> isAlpha == Data.Char.isAlpha
--
{-# INLINE isAlpha #-}
isAlpha :: Char -> Bool
isAlpha = P.isAlphabetic

-- | Returns 'True' for upper-case or title-case letters.  Title case is used by
-- a small number of letter ligatures like the single-character form of /Lj/.
--
-- prop> isUpper == Data.Char.isUpper
--
{-# INLINE isUpper #-}
isUpper :: Char -> Bool
isUpper = P.isUppercase

-- | Returns 'True' for lower-case letters.
--
-- prop> isLower == Data.Char.isLower
--
{-# INLINE isLower #-}
isLower :: Char -> Bool
isLower = P.isLowercase

-- | Returns 'True' for any whitespace characters, and the control
-- characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.
--
-- prop> isSpace == Data.Char.isSpace
--
{-# INLINE isSpace #-}
isSpace :: Char -> Bool
isSpace = P.isWhite_Space
