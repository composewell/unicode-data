import Data.Proxy (Proxy(..))
import Test.Tasty (askOption)
import Test.Tasty.Bench (bgroup, benchIngredients)
import Test.Tasty.Ingredients.Basic (includingOptions)
import Test.Tasty.Options (OptionDescription(..))
import Test.Tasty.Runners (TestTree, defaultMainWithIngredients)

import Unicode.Char.Bench (CharRange(..))
import qualified Unicode.Char.CaseBench as C
import qualified Unicode.Char.Case.CompatBench as CC
import qualified Unicode.Char.GeneralBench as G
import qualified Unicode.Char.General.BlocksBench as B
import qualified Unicode.Char.General.CompatBench as GC
import qualified Unicode.Char.IdentifiersBench as I
import qualified Unicode.Char.NormalizationBench as N
import qualified Unicode.Char.NumericBench as Num
import qualified Unicode.Char.Numeric.CompatBench as NumCompat

main :: IO ()
main = do
  let customOpts  = [Option (Proxy :: Proxy CharRange)]
      ingredients = includingOptions customOpts : benchIngredients
  defaultMainWithIngredients ingredients (askOption benchmarks)

benchmarks :: CharRange -> TestTree
benchmarks charRange = bgroup "All"
  [ C.benchmarks charRange
  , CC.benchmarks charRange
  , G.benchmarks charRange
  , B.benchmarks charRange
  , GC.benchmarks charRange
  , I.benchmarks charRange
  , N.benchmarks charRange
  , Num.benchmarks charRange
  , NumCompat.benchmarks charRange
  ]
