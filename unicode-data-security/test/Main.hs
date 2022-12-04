module Main where

import Test.Hspec
import qualified Unicode.Char.Identifiers.SecuritySpec as Security

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Unicode.Char.Identifiers.Security" Security.spec
