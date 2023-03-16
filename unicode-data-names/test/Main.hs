{-# LANGUAGE CPP #-}

module Main where

import Test.Hspec ( Spec, hspec, describe )
import qualified Unicode.Char.General.NamesSpec as String
#ifdef HAS_TEXT
import qualified Unicode.Char.General.Names.TextSpec as Text
#endif
#ifdef HAS_BYTESTRING
import qualified Unicode.Char.General.Names.ByteStringSpec as ByteString
#endif
#ifdef HAS_ICU
import qualified ICU.NamesSpec as ICU
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Unicode.Char.General.Names" String.spec
#ifdef HAS_BYTESTRING
    describe "Unicode.Char.General.Names.ByteString" ByteString.spec
#endif
#ifdef HAS_TEXT
    describe "Unicode.Char.General.Names.Text" Text.spec
#endif
#ifdef HAS_ICU
    describe "ICU.Names" ICU.spec
#endif
