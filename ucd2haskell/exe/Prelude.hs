{-# Language NoImplicitPrelude, PackageImports #-}

-- |
-- Copyright   : (c) 2025 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--

module Prelude
    ( module Prelude
    , Foldable (..)
    ) where

-- Ensure we always import the whole Foldable class
import "base" Prelude hiding (Foldable (..))
import Data.Foldable (Foldable (..))
