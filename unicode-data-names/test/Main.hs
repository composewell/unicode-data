{-# LANGUAGE CPP #-}

module Main where

import Test.Hspec ( Spec, hspec, describe )
import qualified Unicode.Char.General.NamesSpec as String
#ifdef HAS_ICU
import qualified ICU.NamesSpec as ICU
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Unicode.Char.General.Names" String.spec
#ifdef HAS_ICU
    describe "ICU.Names" ICU.spec
#endif
