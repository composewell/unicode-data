module Main where

import Test.Hspec ( Spec, hspec, describe )
import qualified Unicode.Char.General.NamesSpec as String

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Unicode.Char.General.Names" String.spec
