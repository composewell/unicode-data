-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.UnicodeData.CombiningClass
    ( recipe
    , parseCombining) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Char (ord)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Props
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD

import UCD2Haskell.Common (Fold (..), showB)
import UCD2Haskell.Generator (
    FileRecipe (..),
    ShamochuCode (..),
    apacheLicense,
    genBitmapShamochu,
    mkImports,
    unlinesBB,
 )

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
            , "{-# OPTIONS_HADDOCK hide #-}"
            , "module " <> moduleName
            , "(combiningClass, isCombining)"
            , "where"
            , ""
            , mkImports imports
            , "combiningClass :: Char -> Int"
            , "combiningClass = \\case"
            , unlinesBB (reverse combiningClasses)
            , "  _ -> 0\n"
            , ""
            , code
            ]
        where
        ShamochuCode{..} = genBitmapShamochu
            "isCombining"
            (NE.singleton 6)
            [2,3,4,5,6]
            (reverse combiningCodePoints)

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
