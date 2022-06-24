-- [TODO] @since
-- |
-- Module      : Unicode.Char.General
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode blocks related functions.
--

module Unicode.Char.General.Blocks
    ( B.Block(..)
    , B.BlockDefinition(..)
    , block
    , B.blockDefinition
    , inBlock
    , allBlockRanges
    , allBlockRanges'
    )

where

import Control.Arrow ((***))
import Data.Char (chr, ord)
import Data.Ix (inRange)
import qualified Unicode.Internal.Char.Blocks as B

-- [TODO] @since
-- | Character block, if defined.
{-# INLINE block #-}
block :: Char -> Maybe B.Block
block = fmap toEnum . B.block

-- [TODO] @since
-- | Check if a character is in a block.
{-# INLINE inBlock #-}
inBlock :: B.Block -> Char -> Bool
inBlock b = inRange (B.blockRange (B.blockDefinition b)) . ord

-- [TODO] @since
-- | All the block ranges, in ascending order.
{-# INLINE allBlockRanges #-}
allBlockRanges :: [(Int, Int)]
allBlockRanges = B.allBlockRanges

-- [TODO] @since
-- | Variant of 'allBlockRanges', with ranges expressed as 'Char's instead of
-- 'Int's.
{-# INLINE allBlockRanges' #-}
allBlockRanges' :: [(Char, Char)]
allBlockRanges' = (chr *** chr) <$> B.allBlockRanges
