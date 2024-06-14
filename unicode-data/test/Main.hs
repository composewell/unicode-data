{-# LANGUAGE CPP #-}

module Main where

import Test.Hspec
import qualified Unicode.CharSpec
#ifdef HAS_ICU
import qualified ICU.CharSpec as ICU
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Unicode.Char" Unicode.CharSpec.spec
#ifdef HAS_ICU
    describe "ICU.Char" ICU.spec
#endif
