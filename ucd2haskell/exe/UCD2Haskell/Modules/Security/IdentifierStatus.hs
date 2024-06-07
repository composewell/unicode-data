-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.Security.IdentifierStatus (recipe) where

import qualified Data.ByteString.Builder as BB
import Data.Char (ord)
import Data.Foldable (Foldable(..))
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense, genBitmap)
import UCD2Haskell.Common (Fold (..))

recipe :: FileRecipe Prop.Entry
recipe = ModuleRecipe
    "Unicode.Internal.Char.Security.IdentifierStatus"
    genIdentifierStatusModule

genIdentifierStatusModule :: BB.Builder -> Fold Prop.Entry BB.Builder
genIdentifierStatusModule moduleName = Fold step mempty done
    where

    step acc Prop.Entry{..} = case range of
        U.SingleChar c -> addAllowed value acc c
        U.CharRange{..} -> foldl' (addAllowed value) acc [start..end]

    addAllowed = \case
        "Allowed" -> \acc -> (: acc) . ord
        x         -> error ("Unexpected " <> show x)

    done values = unlinesBB
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(isAllowedInIdentifier)"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Data.Word (Word8)"
        , "import GHC.Exts (Ptr(..))"
        , "import Unicode.Internal.Bits (lookupBit64)"
        , ""
        , genBitmap "isAllowedInIdentifier" (reverse values)
        ]
