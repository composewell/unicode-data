-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.Security.IntentionalConfusables (recipe) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop

import UCD2Haskell.Common (Fold (..), showB)
import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense)

recipe :: FileRecipe Prop.Entry
recipe = ModuleRecipe
    "Unicode.Internal.Char.Security.IntentionalConfusables"
    generate

generate :: BB.Builder -> Fold Prop.Entry BB.Builder
generate moduleName = Fold step mempty done
    where

    step acc Prop.Entry{..} = case range of
        U.SingleChar c1 -> Map.insertWith (flip (<>)) c1 (Set.singleton c2)
                         . Map.insertWith (flip (<>)) c2 (Set.singleton c1)
                         $ acc
        U.CharRange{} -> error ("unexpected range: " <> show (range, value))
        where c2 = U.parseCodePoint value

    mkConfusable :: Char -> Set.Set Char -> BB.Builder
    mkConfusable c cs = mconcat
        [ "\n    "
        , showB c
        , " -> \""
        , stringToAddrLiteral (Set.toList cs)
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
        , "(intentionalConfusables)"
        , "where"
        , ""
        , "import GHC.Exts (Addr#)"
        , ""
        , "-- | Returns the /intentional/ confusables of a character, if any."
        , "--"
        , "-- The resulting 'CString' is null-terminated and encoded in UTF-8."
        , "--"
        , "-- @since 0.1.0"
        , "intentionalConfusables :: Char -> Addr#"
        , "intentionalConfusables = \\case" <> Map.foldMapWithKey mkConfusable confusables
        , "    _ -> \"\\0\"#"
        ]
