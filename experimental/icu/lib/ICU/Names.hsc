-- |
-- Module      : ICU.Names
-- Copyright   : (c) 2023 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode character names and name aliases.
-- See Unicode standard 15.0.0, section 4.8.
--
-- @since 0.3.0

module ICU.Names
    ( name
    , correctedName
    ) where

import Control.Applicative (Alternative(..))
import Data.Char (ord)
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCStringLen)
import Foreign.Marshal.Array (allocaArray)
import System.IO.Unsafe (unsafePerformIO)

type UChar32 = Word32

foreign import ccall unsafe "icu.h __hs_u_charName" u_charName
    :: UChar32 -> Int -> CString -> Int32 -> IO Int32

foreign import capi "icu.h value __hs_U_UNICODE_CHAR_NAME" charNameChoice :: Int
foreign import capi "icu.h value __hs_U_CHAR_NAME_ALIAS" charNameAliasChoice :: Int

charName :: Int -> Char -> Maybe String
charName ty c
    = unsafePerformIO
    . allocaArray bufferLength $
        \ptr -> u_charName cp ty ptr (fromIntegral bufferLength) >>=
        \case
            0   -> pure Nothing
            len -> Just <$> peekCStringLen (ptr, fromIntegral len)
    where
    cp = fromIntegral (ord c)
    bufferLength = 100

name :: Char -> Maybe String
name = charName charNameChoice

correctedName :: Char -> Maybe String
correctedName c = charName charNameAliasChoice c <|> name c
