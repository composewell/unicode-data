-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.Security.Confusables (recipe) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop

import UCD2Haskell.Common (Fold (..), showB)
import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense)

recipe :: FileRecipe Prop.EntryMultipleValues
recipe = ModuleRecipe
    "Unicode.Internal.Char.Security.Confusables"
    genConfusablesModule

genConfusablesModule :: BB.Builder -> Fold Prop.EntryMultipleValues BB.Builder
genConfusablesModule moduleName = Fold step mempty done
    where

    step acc Prop.EntryMultipleValues{..} = case range of
        U.SingleChar c -> Map.insert
            c
            (U.parseCodePointList (NE.head values))
            acc
        U.CharRange{} -> error ("unexpected range: " <> show (range, values))

    mkConfusable :: Char -> String -> BB.Builder
    mkConfusable c s = mconcat
        [ "\n    "
        , showB c
        , " -> \""
        , stringToAddrLiteral s
        , "\\0\"#"
        ]

    -- Encode string with UTF-8
    stringToAddrLiteral = foldMap toEscapedWord8 . encodeUtf8
    encodeUtf8 = BL.unpack . BB.toLazyByteString . BB.stringUtf8
    toEscapedWord8 = (BB.char7 '\\' <>) . BB.word8Dec

    done confusables = unlinesBB
        [ apacheLicense 2022 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(confusablePrototype)"
        , "where"
        , ""
        , "import GHC.Exts (Addr#)"
        , ""
        , "-- | Returns the /prototype/ of a character, if it is confusable."
        , "--"
        , "-- The resulting 'CString' is null-terminated and encoded in UTF-8."
        , "--"
        , "-- @since 0.1.0"
        , "confusablePrototype :: Char -> Addr#"
        , "confusablePrototype = \\case" <> Map.foldMapWithKey mkConfusable confusables
        , "    _ -> \"\\0\"#"
        ]
