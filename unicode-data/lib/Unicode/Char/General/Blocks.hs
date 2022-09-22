-- |
-- Module      : Unicode.Char.General
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode [blocks](https://www.unicode.org/glossary/#block) related functions.
--
-- @since 0.3.1

module Unicode.Char.General.Blocks
    ( B.Block(..)
    , B.BlockDefinition(..)
    , block
    , B.blockDefinition
    , allBlockRanges
    )

where

import qualified Unicode.Internal.Char.Blocks as B

-- | Character [block](https://www.unicode.org/glossary/#block), if defined.
--
-- @since 0.3.1
{-# INLINE block #-}
block :: Char -> Maybe B.Block
block = fmap toEnum . B.block

-- | All the [block](https://www.unicode.org/glossary/#block) ranges,
-- in ascending order.
--
-- @since 0.3.1
{-# INLINE allBlockRanges #-}
allBlockRanges :: [(Int, Int)]
allBlockRanges = B.allBlockRanges
