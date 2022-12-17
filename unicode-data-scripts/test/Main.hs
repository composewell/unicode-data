module Main where

import Test.Hspec
import qualified Unicode.Char.General.ScriptsSpec as Scripts

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Unicode.Char.General.Scripts" Scripts.spec
