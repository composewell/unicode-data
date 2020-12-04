-- |
-- Module      : Unicode.Properties.CombiningClass
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- From the Unicode standard,
--
-- Each character in the Unicode Standard has a combining class associated with
-- it.
--
-- The combining class is a numerical value used by the Canonical Ordering
-- Algorithm to determine which sequences of combining marks are to be
-- considered canonically equivalent and which are not.
--
-- You can read more about it in the Unicode standard. The latest Unicode
-- standard can be found here: <http://www.unicode.org/versions/latest/>.
--
module Unicode.Properties.CombiningClass
    ( combiningClass
    , isCombining
    )
where

import qualified Unicode.Internal.Generated.UnicodeData.CombiningClass as C

-- | Get the combining class of a Unicode character.
{-# INLINE combiningClass #-}
combiningClass :: Char -> Int
combiningClass = C.combiningClass

-- | Check if a character is able to combine with another character.
{-# INLINE isCombining #-}
isCombining :: Char -> Bool
isCombining = C.isCombining
