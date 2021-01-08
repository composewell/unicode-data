-- |
-- Module      : Unicode.UCD.Compose
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- From the Unicode standard,
--
-- A character is a starter if it has a combining class of zero in the Unicode
-- Character Database. Any other character is a non-starter.
--
-- A character is a second starter if it is a starter and can be combined into
-- another starter.
--
-- You can read more about it in the Unicode standard. The latest Unicode
-- standard can be found here: <http://www.unicode.org/versions/latest/>.
--
module Unicode.UCD.Compose
    ( compose
    , composeStarters
    , isSecondStarter
    )
where

import qualified Unicode.Internal.Generated.UnicodeData.Compositions as C

-- | Try to compose a starter with a non-starter.
{-# INLINE compose #-}
compose :: Char -> Char -> Maybe Char
compose = C.compose

-- | Try to compose a starter with a second starter.
{-# INLINE composeStarters #-}
composeStarters :: Char -> Char -> Maybe Char
composeStarters = C.composeStarters

-- | Check if a starter behaves like a second starter.
{-# INLINE isSecondStarter #-}
isSecondStarter :: Char -> Bool
isSecondStarter = C.isSecondStarter
