-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.UnicodeData.Decomposition
    ( -- * Recipes
      decomposable
    , decomposableK
    , decompositions
    , decompositionsK2
    , decompositionsK
      -- * Helpers
    , DType(..)
    , hasDecomposableType
    ) where

import qualified Data.ByteString.Builder as BB
import Data.Char (ord)
import Data.Foldable (Foldable (..))
import qualified Data.List.NonEmpty as NE
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD

import UCD2Haskell.Common (Fold (..), allRange, filterFold, filterNonHangul, showB)
import UCD2Haskell.Generator (
    FileRecipe (..),
    apacheLicense,
    genBitmapShamochu,
    unlinesBB,
    unwordsBB,
 )

data DType = Canonical | Kompat

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

decomposable :: FileRecipe UD.Entry
decomposable = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.Decomposable"
    (`genDecomposableModule` Canonical)

decomposableK :: FileRecipe UD.Entry
decomposableK = ModuleRecipe
     "Unicode.Internal.Char.UnicodeData.DecomposableK"
    (`genDecomposableModule` Kompat)

genDecomposableModule
    :: BB.Builder
    -> DType
    -> Fold UD.Entry BB.Builder
genDecomposableModule moduleName dtype
    = filterNonHangul
    . filterDecomposableType dtype
    $ Fold step initial done

    where

    initial :: [Int]
    initial = []

    step acc (UD.Entry range _) =
        case range of
            U.SingleChar c -> ord c : acc
            U.CharRange{..} -> foldl' (\a c -> ord c : a) acc [start..end]

    done st =
        unlinesBB
            [ apacheLicense 2020 moduleName
            , "{-# OPTIONS_HADDOCK hide #-}"
            , ""
            , "module " <> moduleName
            , "(isDecomposable)"
            , "where"
            , ""
            , "import Data.Bits (Bits(..))"
            , "import Data.Char (ord)"
            , "import Data.Int (Int8)"
            , "import Data.Word (Word8, Word16)"
            , "import GHC.Exts (Ptr(..))"
            , "import Unicode.Internal.Bits (lookupBit, lookupWord8AsInt, lookupWord16AsInt)"
            , ""
            , genBitmapShamochu
                    "isDecomposable"
                    (NE.singleton 6)
                    [2,3,4,5,6]
                    (reverse st)
            ]

filterDecomposableType :: DType -> Fold UD.Entry a -> Fold UD.Entry a
filterDecomposableType dtype =
    filterFold (hasDecomposableType dtype . UD.decomposition . UD.details)

hasDecomposableType :: DType -> UD.Decomposition -> Bool
hasDecomposableType dtype = \case
        UD.Self -> False
        UD.Decomposition t _ -> case dtype of
            Canonical -> t == UD.Canonical
            Kompat -> True

--------------------------------------------------------------------------------
-- Decompositions
--------------------------------------------------------------------------------

decompositions :: FileRecipe UD.Entry
decompositions = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.Decompositions"
    let post = ["  c -> [c]"]
    in (\m -> genDecomposeDefModule m [] post Canonical (const True))

decompositionsK2 :: FileRecipe UD.Entry
decompositionsK2 = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.DecompositionsK2"
    let post = ["  c -> [c]"]
    in (\m -> genDecomposeDefModule m [] post Kompat (>= '\60000'))

decompositionsK :: FileRecipe UD.Entry
decompositionsK = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.DecompositionsK"
    let pre = [ unwordsBB [ "import qualified"
                          , BB.string7 (moduleName decompositionsK2)
                          , "as DK2" ]
              , "" ]
        post = ["  c -> DK2.decompose c"]
    in (\m -> genDecomposeDefModule m pre post Kompat (< '\60000'))

genDecomposeDefModule
    :: BB.Builder
    -> [BB.Builder]
    -> [BB.Builder]
    -> DType
    -> (Char -> Bool)
    -> Fold UD.Entry BB.Builder
genDecomposeDefModule moduleName before after dtype predicate
    = filterFold predicate'
    . filterNonHangul
    . filterDecomposableType dtype
    $ Fold step initial done

    where

    predicate' = allRange predicate . UD.range

    decomposeChar c = \case
        UD.Self -> [c]
        UD.Decomposition _ ds -> ds

    genHeader =
        [ apacheLicense 2020 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(decompose)"
        , "where"
        , ""
        ]
    genSign =
        [ "{-# NOINLINE decompose #-}"
        , "decompose :: Char -> [Char]"
        , "decompose = \\case"
        ]

    initial :: [BB.Builder]
    initial = []

    step acc (UD.Entry range details) = case range of
        U.SingleChar c -> step' (UD.decomposition details) acc c
        U.CharRange start end -> foldl'
            (step' (UD.decomposition details))
            acc
            [start..end]
    step' decomp acc c = genDecomposeDef c decomp : acc

    done st =
        let body = mconcat [genHeader, before, genSign, reverse st, after]
        in unlinesBB body

    genDecomposeDef c decomp = mconcat
        [ "  "
        , showB c
        , " -> "
        , showB (decomposeChar c decomp)
        ]
