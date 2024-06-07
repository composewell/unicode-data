-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.UnicodeData.SimpleCaseMappings
    ( upperRecipe
    , lowerRecipe
    , titleRecipe
    ) where

import qualified Data.ByteString.Builder as BB
import Data.Functor ((<&>), ($>))
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense)
import UCD2Haskell.Common (Fold (..))

upperRecipe :: FileRecipe UD.Entry
upperRecipe = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.SimpleUpperCaseMapping"
    (\m -> genSimpleCaseMappingModule m "toSimpleUpperCase" UD.simpleUpperCaseMapping)

lowerRecipe  :: FileRecipe UD.Entry
lowerRecipe = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.SimpleLowerCaseMapping"
    (\m -> genSimpleCaseMappingModule m "toSimpleLowerCase" UD.simpleLowerCaseMapping)

titleRecipe :: FileRecipe UD.Entry
titleRecipe = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.SimpleTitleCaseMapping"
    (\m -> genSimpleCaseMappingModule m "toSimpleTitleCase" UD.simpleTitleCaseMapping)

genSimpleCaseMappingModule
    :: BB.Builder
    -> BB.Builder
    -> (UD.CharDetails -> Maybe Char)
    -> Fold UD.Entry BB.Builder
genSimpleCaseMappingModule moduleName funcName field =
    Fold step initial done

    where

    genHeader =
        [ apacheLicense 2020 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(" <> funcName <> ")"
        , "where"
        , ""
        ]
    genSign =
        [ "{-# NOINLINE " <> funcName <> " #-}"
        , funcName <> " :: Char -> Char"
        , funcName <> " = \\case"
        ]
    initial = []

    step ds dc = case mkEntry dc of
        Nothing -> ds
        Just d  -> d : ds

    after = ["  c -> c"]

    done st =
        let body = mconcat [genHeader, genSign, reverse st, after]
        in unlinesBB body

    mkEntry (UD.Entry r dc) = case r of
      U.SingleChar ch -> field dc <&> \c -> mconcat
        [ "  "
        , BB.string7 . show $ ch
        , " -> "
        , BB.string7 . show $ c
        ]
        -- TODO: switch to hexadecimal formatting for better debugging?
        --       what about code size increase?
        -- [ "  '\\x"
        -- , showHexChar ch
        -- , "' -> '\\x"
        -- , showHexChar c
        -- , "'"
        -- ]
        -- where showHexChar = BB.wordHex . fromIntegral . ord
      U.CharRange{} -> field dc $> error ("genSimpleCaseMappingModule: unexpected char range: " <> show r)

