{-# LANGUAGE BlockArguments #-}

module Unicode.Char.General.ScriptsSpec
  ( spec
  ) where

import qualified Unicode.Char.General.Scripts as UScripts
import Data.Foldable (traverse_)
import Test.Hspec

{- [NOTE]
These tests may fail if the compiler’s Unicode version
does not match the version of this package.

+-------------+----------------+-----------------+
| GHC version | @base@ version | Unicode version |
+=============+================+=================+
| 8.8         | 4.13           | 12.0            |
| 8.10.[1-4]  | 4.14.{0,1}     | 12.0            |
| 8.10.5+     | 4.14.2+        | 13.0            |
| 9.0.1       | 4.15.0         | 12.1            |
| 9.2.1       | 4.16.0         | 14.0            |
+-------------+----------------+-----------------+
-}

spec :: Spec
spec = do
  describe "Unicode scripts" do
    it "inScript"
        let check s = if all (UScripts.inScript s) (UScripts.scriptDefinition s)
                    then pure ()
                    else expectationFailure (show s)
        in traverse_ check [minBound..maxBound]
    it "Characters are in the definition of their corresponding script"
        let {
            check c = let s = UScripts.script c in if UScripts.inScript s c
                    then pure ()
                    else expectationFailure $ mconcat
                        [ "Char “", show c, "” in not in the definition of “"
                        , show s, "”." ]
        } in traverse_ check [minBound..maxBound]
    it "Characters in a script definition have the corresponding script"
        let {
            checkChar s c = let s' = UScripts.script c in if s' == s
                then pure ()
                else expectationFailure $ mconcat
                    [ "Script is different for “", show c, "”. Expected: “"
                    , show s, "” but got: “", show s', "”." ];
            check s = let chars = UScripts.scriptDefinition s
                      in traverse_ (checkChar s) chars
        } in traverse_ check [minBound..maxBound]
