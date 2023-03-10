{-# LANGUAGE CPP #-}

module Main where

import Test.Hspec
import qualified Unicode.Char.General.ScriptsSpec as Scripts
#ifdef HAS_ICU
import qualified Unicode.Char.General.Scripts.ICUSpec as ICU
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Unicode.Char.General.Scripts" Scripts.spec
#ifdef HAS_ICU
    describe "Unicode.Char.General.Scripts.ICU" ICU.spec
#endif
