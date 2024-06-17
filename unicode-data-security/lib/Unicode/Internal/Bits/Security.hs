{-# LANGUAGE CPP #-}

-- |
-- Module      : Unicode.Internal.Bits.Security
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Compatibility module

module Unicode.Internal.Bits.Security
    ( unpackCStringUtf8#
    ) where

#if MIN_VERSION_base(4,15,0)
import GHC.Exts (unpackCStringUtf8#)
#else
import GHC.CString (unpackCStringUtf8#)
#endif
