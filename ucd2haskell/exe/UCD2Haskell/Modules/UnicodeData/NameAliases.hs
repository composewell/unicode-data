-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.UnicodeData.NameAliases (recipe) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import qualified Unicode.CharacterDatabase.Parser.NameAliases as N

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense, enumMapToAddrLiteral8)
import UCD2Haskell.Common (Fold (..), showHexCodepointB)

recipe :: FileRecipe N.Entry
recipe = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.NameAliases"
    genAliasesModule

type CharAliases = Map.Map N.AliasType [BS.ShortByteString]
type Aliases = Map.Map Char CharAliases

genAliasesModule :: BB.Builder -> Fold N.Entry BB.Builder
genAliasesModule moduleName = Fold step mempty done
    where

    step :: Aliases -> N.Entry -> Aliases
    step acc N.Entry{..} = Map.alter
        (Just . \case
            Nothing -> Map.singleton nameAliasType [nameAlias]
            Just as -> Map.insertWith (flip (<>)) nameAliasType [nameAlias] as)
        char
        acc

    mkCharAliases :: Char -> CharAliases -> BB.Builder
    mkCharAliases char aliases = mconcat
        [ "\n  '\\x"
        , showHexCodepointB char
        , "'# -> \""
        , mkCharAliasesLiteral char aliases
        , "\"#"
        ]

    mkCharAliasesLiteral :: Char -> CharAliases -> BB.Builder
    mkCharAliasesLiteral char aliasesMap =
        enumMapToAddrLiteral8 0 0xfff
            (reverse index)
            (mconcat (reverse ("\\0":aliases)))
        where
        (index, aliases, _) = foldl'
            (\acc ty -> addAliasType char acc ty (Map.findWithDefault mempty ty aliasesMap))
            (mempty, mempty, typesCount)
            [minBound..maxBound]

    typesCount = fromEnum (maxBound :: N.AliasType)
               - fromEnum (minBound :: N.AliasType)
               + 1

    -- [FIXME] [(Word8:AliasType,Word8:index of first alias)] [CString]
    addAliasType
        :: Char
        -> ([Word8], [BB.Builder], Int) -- (index, aliases, last alias index)
        -> N.AliasType
        -> [BS.ShortByteString]
        -> ([Word8], [BB.Builder], Int)
    addAliasType char (index, aliasesAcc, lastAliasIndex) _ty = \case
        [] ->
            ( 0 : index
            , aliasesAcc
            , lastAliasIndex )
        aliases -> if lastAliasIndex < 0xff
            then
                ( fromIntegral lastAliasIndex : index
                , encodedAliases
                , lastAliasIndex' )
            else error . mconcat $
                [ "Cannot encode char ", show char
                , "aliases. Offset: ", show lastAliasIndex, " >= 0xff" ]
            where
            (encodedAliases, lastAliasIndex') =
                addEncodedAliases (aliasesAcc, lastAliasIndex) aliases
            addEncodedAliases (as, offset) = \case
                alias : rest -> addEncodedAliases
                    ( mconcat ["\\", BB.intDec len, BB.shortByteString alias] : as
                    , offset' )
                    rest
                    where
                    len = BS.length alias
                    offset' = offset + len + 1
                [] -> ("\\0" : as, offset + 1)

    done names = unlinesBB
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE DeriveGeneric, PatternSynonyms #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(NameAliasType(..), pattern MaxNameAliasType, nameAliases)"
        , "where"
        , ""
        , "import Data.Ix (Ix)"
        , "import GHC.Exts (Addr#, Char#, Int#)"
        , "import GHC.Generics (Generic)"
        , ""
        , "-- | Type of name alias. See Unicode Standard 15.0.0, section 4.8."
        , "--"
        , "-- @since 0.1.0"
        , "data NameAliasType"
        , "    = Correction"
        , "    -- ^ Corrections for serious problems in the character names."
        , "    | Control"
        , "    -- ^ ISO&#xa0;6429 names for @C0@ and @C1@ control functions, and other"
        , "    --   commonly occurring names for control codes."
        , "    | Alternate"
        , "    -- ^ A few widely used alternate names for format characters."
        , "    | Figment"
        , "    -- ^ Several documented labels for @C1@ control code points which"
        , "    --   were never actually approved in any standard."
        , "    | Abbreviation"
        , "    -- ^ Commonly occurring abbreviations (or acronyms) for control codes,"
        , "    --   format characters, spaces, and variation selectors."
        , "    deriving (Generic, Enum, Bounded, Eq, Ord, Ix, Show)"
        , ""
        , "-- $setup"
        , "-- >>> import GHC.Exts (Int(..))"
        , ""
        , "-- |"
        , "-- >>> I# MaxNameAliasType == fromEnum (maxBound :: NameAliasType)"
        , "-- True"
        , "pattern MaxNameAliasType :: Int#"
        , "pattern MaxNameAliasType = "
            <> BB.intDec (fromEnum (maxBound :: N.AliasType)) <> "#"
        , ""
        , "-- | Detailed character names aliases."
        , "-- The names are listed in the original order of the UCD."
        , "--"
        , "-- Encoding:"
        , "--"
        , "-- * If there is no alias, return @\"\\\\xff\"#@."
        , "-- * For each type of alias, the aliases are encoded as list of (length, alias)."
        , "--   The list terminates with @\\\\0@."
        , "-- * The list are then concatenated in order of type of alias and"
        , "--   terminates with @\\\\0@."
        , "-- * The first "
            <> BB.intDec (fromEnum (maxBound :: N.AliasType) + 1)
            <> " bytes represent each one the index of the first element of the"
        , "--   corresponding list of aliases. When the list is empty, then the index is 0."
        , "-- * Example: @\\\"\\\\5\\\\0\\\\13\\\\0\\\\0\\\\3XXX\\\\2YY\\\\0\\\\4ZZZZ\\\\0\\\\0\\\"#@"
        , "--   represents: @[('Correction',[\\\"XXX\\\", \\\"YY\\\"]),('Alternate', [\\\"ZZZZ\\\"])]@."
        , "--"
        , "-- @since 0.1.0"
        , "nameAliases :: Char# -> Addr#"
        , "nameAliases = \\case" <> Map.foldMapWithKey mkCharAliases names
        , "  _          -> \"\\xff\"#"
        ]
