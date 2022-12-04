module Main where

import Test.Hspec
import qualified Unicode.CharSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Unicode.Char" Unicode.CharSpec.spec
