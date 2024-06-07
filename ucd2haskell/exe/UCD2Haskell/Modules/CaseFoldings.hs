-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.CaseFoldings (recipe) where

import Control.Applicative (Alternative(..))
import Data.Bits (Bits(..))
import qualified Data.ByteString.Builder as BB
import Data.Char (ord)
import qualified Data.Map.Strict as Map
import qualified Unicode.CharacterDatabase.Parser.CaseFolding as C

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense)
import UCD2Haskell.Common (Fold (..), showB)

recipe :: FileRecipe C.Entry
recipe = ModuleRecipe
    "Unicode.Internal.Char.CaseFolding"
    genCaseFolding

-- [NOTE] Case folding encodes up to 3 code points on 21 bits each in an Int64.
genCaseFolding :: BB.Builder -> Fold C.Entry BB.Builder
genCaseFolding moduleName = Fold step mempty done
    where
    step acc C.Entry{..} = Map.alter
        (Just . \case
            Nothing -> Map.singleton caseFoldingType caseFolding
            Just cf -> Map.insert caseFoldingType caseFolding cf)
        char
        acc

    done acc = unlinesBB
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(toCasefold)"
        , "where"
        , ""
        , "import Data.Int (Int64)"
        , ""
        , "{-# NOINLINE toCasefold #-}"
        , "toCasefold :: Char -> Int64"
        , "toCasefold = \\case" <> Map.foldlWithKey' addEntry mempty acc
        , "  _ -> 0"
        ]

    addEntry acc c cfs =
        case Map.lookup C.FullCaseFolding cfs <|> Map.lookup C.CommonCaseFolding cfs of
            Nothing -> acc
            Just cf -> acc <> mconcat
                [ "\n  "
                , showB c
                , " -> 0x"
                , BB.wordHex (fromIntegral (encode cf))
                ]

    encode :: String -> Int
    encode
        = foldr (\(k, c) -> (+) (ord c `shiftL` k)) 0
        . zip [0, 21, 42]
        -- Check min 1 character, max 3 characters
        . (\cs -> if null cs || length cs > 3 then error (show cs) else cs)
