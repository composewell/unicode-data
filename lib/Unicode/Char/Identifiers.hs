-- |
-- Module      : Unicode.Char.Identifiers
-- Copyright   : (c) 2021 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode Identifier and Pattern Syntax property functions based on
-- [Unicode Standard Annex #31](https://www.unicode.org/reports/tr31/)

module Unicode.Char.Identifiers
    ( isIDContinue
    , isIDStart
    , isXIDContinue
    , isXIDStart
    , isPatternSyntax
    , isPatternWhitespace
    )
where

import qualified Unicode.Internal.Char.DerivedCoreProperties as P
import qualified Unicode.Internal.Char.PropList as P

-- | Returns 'True' if a character is an identifier continue character.
--
-- @since 0.2.0
{-# INLINE isIDContinue #-}
isIDContinue :: Char -> Bool
isIDContinue = P.isID_Continue

-- | Returns 'True' if a character is an identifier start character.
--
-- @since 0.2.0
{-# INLINE isIDStart #-}
isIDStart :: Char -> Bool
isIDStart = P.isID_Start

-- | Returns 'True' if a character is an identifier continue character,
-- using the NFKC modifications detailed in
-- [UAX #31, 5.1](https://www.unicode.org/reports/tr31/#NFKC_Modifications).
--
-- @since 0.2.0
{-# INLINE isXIDContinue #-}
isXIDContinue :: Char -> Bool
isXIDContinue = P.isXID_Continue


-- | Returns 'True' if a character is an identifier start character,
-- using the NFKC modifications detailed in
-- [UAX #31, 5.1](https://www.unicode.org/reports/tr31/#NFKC_Modifications).
--
-- @since 0.2.0
{-# INLINE isXIDStart #-}
isXIDStart :: Char -> Bool
isXIDStart = P.isXID_Start

-- | Returns 'True' if a character is a pattern syntax character.
--
-- @since 0.2.0
{-# INLINE isPatternSyntax #-}
isPatternSyntax :: Char -> Bool
isPatternSyntax = P.isPattern_Syntax

-- | Returns 'True' if a character is a pattern whitespace character.
--
-- @since 0.2.0
{-# INLINE isPatternWhitespace #-}
isPatternWhitespace :: Char -> Bool
isPatternWhitespace = P.isPattern_White_Space
