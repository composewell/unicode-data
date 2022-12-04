module Main where

import Test.Hspec
import qualified Unicode.Char.General.NamesSpec as Names

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Unicode.Char.General.Names" Names.spec
