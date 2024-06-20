-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.SpecialCasings
    ( upperRecipe
    , lowerRecipe
    , titleRecipe
    , parse
    ) where

import qualified Data.ByteString.Builder as BB
import Data.Char (ord)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.SpecialCasing as SC
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense)
import UCD2Haskell.Common (Fold (..))
import Control.Applicative (Alternative(..))
import Data.Bits (Bits(..))
import qualified Data.ByteString as B

type SpecialCasings = Map.Map Char SC.SpecialCasing

parse :: B.ByteString -> SpecialCasings
parse = foldr addCasings mempty . SC.parse
    where
        addCasings (SC.Entry ch sc)
            -- Do not add locale-specific casings
            | null (SC.conditions sc) = Map.insert ch sc
            | otherwise = id

upperRecipe :: SpecialCasings -> FileRecipe UD.Entry
upperRecipe sc = ModuleRecipe
    "Unicode.Internal.Char.SpecialCasing.UpperCaseMapping"
    (\m -> genSpecialCaseMappingModule m
            "toSpecialUpperCase"
            sc
            SC.upper
            UD.simpleUpperCaseMapping )

lowerRecipe :: SpecialCasings -> FileRecipe UD.Entry
lowerRecipe sc = ModuleRecipe
    "Unicode.Internal.Char.SpecialCasing.LowerCaseMapping"
    (\m -> genSpecialCaseMappingModule m
            "toSpecialLowerCase"
            sc
            SC.lower
            UD.simpleLowerCaseMapping )

titleRecipe :: SpecialCasings -> FileRecipe UD.Entry
titleRecipe sc = ModuleRecipe
    "Unicode.Internal.Char.SpecialCasing.TitleCaseMapping"
    (\m -> genSpecialCaseMappingModule m
            "toSpecialTitleCase"
            sc
            SC.title
            UD.simpleTitleCaseMapping )

-- [NOTE] Case mapping encodes up to 3 code points on 21 bits each in an Int64.
genSpecialCaseMappingModule
    :: BB.Builder
    -> BB.Builder
    -> SpecialCasings
    -- ^ Special casings
    -> (SC.SpecialCasing -> String)
    -- ^ Special case selector
    -> (UD.CharDetails -> Maybe Char)
    -- ^ Simple case selector
    -> Fold UD.Entry BB.Builder
genSpecialCaseMappingModule moduleName funcName specialCasings special simple =
    Fold step initial done

    where

    genHeader =
        [ apacheLicense 2022 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(" <> funcName <> ")"
        , "where"
        , ""
        , "import Data.Int (Int64)"
        , ""
        , "{-# NOINLINE " <> funcName <> " #-}"
        , funcName <> " :: Char -> Int64"
        , funcName <> " = \\case"
        ]
    initial = []

    step xs (UD.Entry range dc) = case range of
        U.SingleChar ch -> case mkEntry ch dc of
            Nothing -> xs
            Just x  -> x : xs
        _ -> xs

    after = ["  _ -> 0"]

    done st =
        let body = mconcat [genHeader, reverse st, after]
        in unlinesBB body

    mkEntry ch dc = (mkSpecial ch <|> mkSimple dc) <&> \cp -> mconcat
        [ "  "
        , BB.string7 . show $ ch
        , " -> 0x"
        , BB.wordHex . fromIntegral $ cp
        ]
    -- TODO: switch to hexadecimal formatting for better debugging?
    --       what about code size increase?
    --     [ "  '\\x"
    --     , showHexCodePoint (ord ch)
    --     , "' -> '\\x"
    --     , showHexCodePoint cp
    --     , "'"
    --     ]
    -- showHexCodePoint = BB.wordHex . fromIntegral

    mkSimple = fmap ord . simple
    mkSpecial = fmap (encode . special) . (specialCasings Map.!?)
    encode :: String -> Int
    encode
        = foldr (\(k, c) -> (+) (ord c `shiftL` k)) 0
        . zip [0, 21, 42]
        -- Check min 1 character, max 3 characters
        . (\cs -> if null cs || length cs > 3 then error (show cs) else cs)
