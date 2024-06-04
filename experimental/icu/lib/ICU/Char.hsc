-- |
-- Module      : ICU.Char
-- Copyright   : (c) 2023 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode character general properties
--
-- @since 0.3.0

module ICU.Char
    ( unicodeVersion
    , charAge
    ) where

import Data.Char (ord)
import Data.Int (Int8)
import Data.Version (Version, makeVersion)
import Data.Word (Word32)
import Foreign (Ptr)
import Foreign.Marshal.Array (allocaArray, peekArray)
import System.IO.Unsafe (unsafePerformIO)

type UChar32 = Word32

foreign import capi "icu.h value __hs_U_MAX_VERSION_LENGTH" maxVersionLength :: Int

foreign import ccall unsafe "icu.h __hs_u_getUnicodeVersion" u_getUnicodeVersion
    :: Ptr Int8 -> IO ()

-- | ICU Unicode version
unicodeVersion :: Version
unicodeVersion
    = makeVersion
    . fmap fromIntegral
    . unsafePerformIO
    $ allocaArray
        maxVersionLength
        (\ptr -> u_getUnicodeVersion ptr *> peekArray maxVersionLength ptr)

foreign import ccall unsafe "icu.h __hs_u_charAge" u_charAge
    :: UChar32 -> Ptr Int8 -> IO ()

-- | Character age
charAge :: Char -> Version
charAge c
    = makeVersion
    . fmap fromIntegral
    . unsafePerformIO
    $ allocaArray
        maxVersionLength
        (\ptr -> u_charAge cp ptr *> peekArray maxVersionLength ptr)
    where
    cp = fromIntegral (ord c)
