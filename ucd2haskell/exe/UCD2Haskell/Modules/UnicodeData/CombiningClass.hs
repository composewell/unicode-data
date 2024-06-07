-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.UnicodeData.CombiningClass
    ( recipe
    , parseCombining) where

import qualified Data.ByteString.Builder as BB
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Props

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense, genBitmap)
import UCD2Haskell.Common (Fold (..), showB)
import Data.Char (ord)
import Data.Foldable (Foldable(..))
import qualified Data.ByteString as B
import qualified Data.Set as Set

recipe :: FileRecipe UD.Entry
recipe = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.CombiningClass"
    genCombiningClassModule

data Acc = Acc
    { combiningClasses :: ![BB.Builder]
    , combiningCodePoints :: ![Int] }

genCombiningClassModule :: BB.Builder -> Fold UD.Entry BB.Builder
genCombiningClassModule moduleName = Fold step initial done
    where

    initial = Acc [] []

    step acc (UD.Entry range details)
        -- Skip non combining
        | cc == 0 = acc
        | otherwise = case range of
            U.SingleChar c -> step' cc acc c
            U.CharRange start end -> foldl' (step' cc) acc [start..end]
        where cc = UD.combiningClass details
    step' cc Acc{..} c = Acc
        { combiningClasses = genCombiningClassDef c cc : combiningClasses
        , combiningCodePoints = ord c : combiningCodePoints }

    done Acc{..} =
        unlinesBB
            [ apacheLicense 2020 moduleName
            , "{-# LANGUAGE LambdaCase #-}"
            , "{-# OPTIONS_HADDOCK hide #-}"
            , "module " <> moduleName
            , "(combiningClass, isCombining)"
            , "where"
            , ""
            , "import Data.Char (ord)"
            , "import Data.Word (Word8)"
            , "import GHC.Exts (Ptr(..))"
            , "import Unicode.Internal.Bits (lookupBit64)"
            , ""
            , "combiningClass :: Char -> Int"
            , "combiningClass = \\case"
            , unlinesBB (reverse combiningClasses)
            , "  _ -> 0\n"
            , ""
            , genBitmap "isCombining" (reverse combiningCodePoints)
            ]

    genCombiningClassDef c cc = mconcat
        [ "  "
        , showB c
        , " -> "
        , BB.word8Dec cc
        ]

parseCombining :: B.ByteString -> Set.Set Char
parseCombining = foldr addCombining mempty . Props.parse
    where
        addCombining Props.Entry{..}
            | value == "0" = id
            | otherwise = case range of
                U.SingleChar c -> Set.insert c
                U.CharRange{..} -> (Set.fromList [start..end] <>)
