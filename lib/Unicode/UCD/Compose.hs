-- |
-- Module      : Unicode.UCD.Compose
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Unicode.UCD.Compose
    ( compose
    , isSecondStarter
    , composeStarters
    )
where

import qualified Unicode.Internal.Generated.UnicodeData.Compositions as C

-- | Compose a starter character (combining class 0) with a combining character
-- (non-zero combining class). Returns the composed character if the starter
-- combines with the combining character, returns 'Nothing' otherwise.
{-# INLINE compose #-}
compose :: Char -> Char -> Maybe Char
compose = C.compose

-- | Compose a starter character with another starter character.  Returns the
-- composed character if the two starters combine, returns 'Nothing' otherwise.
{-# INLINE composeStarters #-}
composeStarters :: Char -> Char -> Maybe Char
composeStarters = C.composeStarters

-- | Return 'True' if a starter character may combine with some preceding
-- starter character.
{-# INLINE isSecondStarter #-}
isSecondStarter :: Char -> Bool
isSecondStarter = C.isSecondStarter
