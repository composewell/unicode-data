-- |
-- Module      : Unicode.UCD.CombiningClass
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Determine the combining properties of characters.
--
module Unicode.UCD.CombiningClass
    ( isCombining
    , combiningClass
    )
where

import qualified Unicode.Internal.Generated.UnicodeData.CombiningClass as C

-- | Returns the combining class of a character.
{-# INLINE combiningClass #-}
combiningClass :: Char -> Int
combiningClass = C.combiningClass

-- | Returns 'True' if a character is a combining character.
{-# INLINE isCombining #-}
isCombining :: Char -> Bool
isCombining = C.isCombining
