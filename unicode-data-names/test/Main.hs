{-# LANGUAGE CPP #-}

module Main where

import Test.Hspec
import qualified Unicode.Char.General.NamesSpec as Names
#ifdef HAS_ICU
import qualified Unicode.Char.General.Names.ICUSpec as ICU
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Unicode.Char.General.Names" Names.spec
#ifdef HAS_ICU
    describe "Unicode.Char.General.Names.ICU" ICU.spec
#endif
