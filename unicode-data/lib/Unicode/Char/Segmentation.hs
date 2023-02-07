-- |
-- Module      : Unicode.Char.Numeric
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Segmentation properties related functions.
--
-- @since 0.4.1

module Unicode.Char.Segmentation
    ( -- * Grapheme Cluster Break
      GraphemeClusterBreak(..)
    , graphemeClusterBreak
    ) where

import Data.Ix (Ix)
import qualified Unicode.Internal.Char.GraphemeBreak as GraphemeBreak

-- | Grapheme Cluster Break classes.
--
-- These classes are defined in the Unicode Standard Annex #29
-- “[Unicode Text Segmentation](https://www.unicode.org/reports/tr29/#Default_Grapheme_Cluster_Table)”.
--
-- __Note:__ the classes must be in the same order they are listed in the
-- Unicode Standard, because some functions (e.g. 'graphemeClusterBreak') rely on the
-- 'Enum' instance.
--
-- @since 0.4.1
data GraphemeClusterBreak
    = CR
    | LF
    | Control
    | Extend
    | ZWJ
    | RegionalIndicator
    | Prepend
    | SpacingMark
    | L
    | V
    | T
    | LV
    | LVT
    | Other
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

-- | Return the @Grapheme_Cluster_Break@ property of a character.
--
-- @since 0.4.1
{-# INLINE graphemeClusterBreak #-}
graphemeClusterBreak :: Char -> GraphemeClusterBreak
graphemeClusterBreak = toEnum . GraphemeBreak.graphemeClusterBreak
