-- |
-- Module      : Unicode.Char.General.Blocks
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode [blocks](https://www.unicode.org/glossary/#block) related functions.
--
-- @since 0.3.1

module Unicode.Char.General.Blocks
    ( -- * Blocks
      B.Block(..)
    , block
      -- * Blocks definitions
    , BlockDefinition(..)
    , blockDefinition
    )

where

import GHC.Exts (Char (..), Int (..), dataToTag#, tagToEnum#)

import Unicode.Internal.Bits (unpackCString#)
import qualified Unicode.Internal.Char.Blocks as B

-- | Character [block](https://www.unicode.org/glossary/#block), if defined.
--
-- @since 0.3.1
{-# INLINE block #-}
block :: Char -> Maybe B.Block
block (C# c#) = case B.block c# of
    -1# -> Nothing
    b#  -> Just (tagToEnum# b# :: B.Block)

-- | Block definition: range and name.
--
-- @since 0.3.1
data BlockDefinition = BlockDefinition
    { blockRange :: !(Int, Int) -- ^ Range
    , blockName :: !String      -- ^ Name
    } deriving (Eq, Ord, Show)

-- | Block definition
--
-- @since 0.3.1
blockDefinition :: B.Block -> BlockDefinition
blockDefinition b = case B.blockDefinition (dataToTag# b) of
    (# lower#, upper#, name# #) -> BlockDefinition range name
        where
        !range = (I# lower#, I# upper#)
        -- Note: names are ASCII. See Unicode Standard 15.0.0, section 3.4.
        !name = unpackCString# name#
