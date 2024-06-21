{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.Security.IdentifierType (recipe) where

import Control.Exception (assert)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
import Data.Foldable (Foldable(..))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop

import UCD2Haskell.Common (Fold (..))
import UCD2Haskell.Generator (
    FileRecipe (..),
    ShamochuCode (..),
    apacheLicense,
    genEnumBitmapShamochu,
    mkImports,
    splitPlanes,
    unlinesBB,
    (<+>),
 )

recipe :: FileRecipe Prop.Entry
recipe = ModuleRecipe
    "Unicode.Internal.Char.Security.IdentifierType"
    genIdentifierTypeModule

data IdentifierType
    = Not_Character
    | Deprecated
    | Default_Ignorable
    | Not_NFKC
    | Not_XID
    | Exclusion
    | Obsolete
    | Technical
    | Uncommon_Use
    | Limited_Use
    | Inclusion
    | Recommended
    deriving (Eq, Ord, Bounded, Enum, Show)

-- TODO: We should really use Set instead of NonEmpty.
--       This is just to maintain compatibility during the migration
newtype IdentifierTypes = IdentifierTypes
    -- TODO { unIdentifierTypes :: Set.Set IdentifierType }
    { unIdentifierTypes :: NE.NonEmpty IdentifierType }
    deriving newtype (Eq, Ord, Semigroup)

parseIdentifierType :: BS.ShortByteString -> IdentifierType
parseIdentifierType = \case
    "Not_Character" -> Not_Character
    "Deprecated" -> Deprecated
    "Default_Ignorable" -> Default_Ignorable
    "Not_NFKC" -> Not_NFKC
    "Not_XID" -> Not_XID
    "Exclusion" -> Exclusion
    "Obsolete" -> Obsolete
    "Technical" -> Technical
    "Uncommon_Use" -> Uncommon_Use
    "Limited_Use" -> Limited_Use
    "Inclusion" -> Inclusion
    "Recommended" -> Recommended
    raw -> error ("parseIdentifierType: Cannot parse: " <> show raw)

genIdentifierTypeModule :: BB.Builder -> Fold Prop.Entry BB.Builder
genIdentifierTypeModule moduleName = Fold step mempty done
    where

    step
        :: Map.Map Char IdentifierTypes
        -> Prop.Entry
        -> Map.Map Char IdentifierTypes
    step acc Prop.Entry{..} = case range of
        U.SingleChar c -> addIdentifierType values acc c
        U.CharRange{..} -> foldl' (addIdentifierType values) acc [start..end]
        where values = NE.fromList (U.parseList value)

    addIdentifierType types acc c = Map.insertWith
        -- TODO (<>)
        (flip (<>))
        c
        -- TODO (IdentifierTypes (Set.fromList (parseIdentifierType <$> NE.toList types)))
        (IdentifierTypes (parseIdentifierType <$> types))
        acc

    mkIdentifiersTypes
        :: Map.Map Char IdentifierTypes
        -> (BB.Builder, [Int], Int)
    mkIdentifiersTypes types =
        let encoding = Set.toList (Set.fromList (def : Map.elems types))
            defIdx = case L.elemIndex def encoding of
                Nothing -> error "impossible"
                Just i -> assert (i == 0) i
        in assert (length encoding < 0xff)
            ( foldMap addEncoding (zip [0..] encoding)
            , snd (Map.foldlWithKey' (addChar defIdx encoding) ('\0', mempty) types)
            , defIdx )

    -- Default value
    -- TODO def = IdentifierTypes (Set.singleton Not_Character)
    def = IdentifierTypes (NE.singleton Not_Character)

    addEncoding :: (Int, IdentifierTypes) -> BB.Builder
    addEncoding (n, e) = mconcat
        [ "\n    "
        , BB.intDec n
        , " -> "
        , mkHaskellConstructorsList e ]

    addChar
        :: Int
        -> [IdentifierTypes]
        -> (Char, [Int])
        -> Char
        -> IdentifierTypes
        -> (Char, [Int])
    addChar defIdx encoding (expected, acc) c types = if expected < c
        then
            let acc' = encodeTypes defIdx encoding def : acc
            in addChar defIdx encoding (succ expected, acc') c types
        else (succ c, encodeTypes defIdx encoding types : acc)

    encodeTypes :: Int -> [IdentifierTypes] -> IdentifierTypes -> Int
    encodeTypes defIdx encoding types =
        fromMaybe defIdx (L.elemIndex types encoding)

    -- HACK: remove underscore to get constructor
    mkHaskellConstructorsList
        = BB.string7
        . filter (/= '_')
        . show
        -- TODO . Set.toList
        . NE.toList
        . unIdentifierTypes

    done acc = unlinesBB
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE OverloadedLists #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(IdentifierType(..), identifierTypes, decodeIdentifierTypes)"
        , "where"
        , ""
        , mkImports (imports <+> Map.singleton "Data.List.NonEmpty" (Set.singleton "NonEmpty"))
        , "-- | Identifier type"
        , "--"
        , "-- @since 0.1.0"
        , "data IdentifierType"
        , "    = NotCharacter"
        , "    -- ^ Unassigned characters, private use characters, surrogates,"
        , "    -- non-whitespace control characters."
        , "    | Deprecated"
        , "    -- ^ Characters with the Unicode property @Deprecated=Yes@."
        , "    | DefaultIgnorable"
        , "    -- ^ Characters with the Unicode property \
                   \@Default_Ignorable_Code_Point=Yes@."
        , "    | NotNFKC"
        , "    -- ^ Characters that cannot occur in strings normalized to NFKC."
        , "    | NotXID"
        , "    -- ^ Characters that do not qualify as default Unicode identifiers;"
        , "    -- that is, they do not have the Unicode property XID_Continue=True."
        , "    | Exclusion"
        , "    -- ^ Characters with @Script_Extensions@ values containing a script"
        , "    -- in /Excluded Scripts/, and no script from /Limited Use Scripts/"
        , "    -- or /Recommended Scripts/, other than “Common” or “Inherited”."
        , "    | Obsolete"
        , "    -- ^ Characters that are no longer in modern use, or that are not"
        , "    -- commonly used in modern text."
        , "    | Technical"
        , "    -- ^ Specialized usage: technical, liturgical, etc."
        , "    | UncommonUse"
        , "    -- ^ Characters that are uncommon, or are limited in use, or"
        , "    -- whose usage is uncertain."
        , "    | LimitedUse"
        , "    -- ^ Characters from scripts that are in limited use."
        , "    | Inclusion"
        , "    -- ^ Exceptionally allowed characters."
        , "    | Recommended"
        , "    -- ^ Characters from scripts that are in widespread everyday common use."
        , "    deriving (Eq, Ord, Bounded, Enum, Show)"
        , ""
        , "-- | Useful to decode the output of 'identifierTypes'."
        , "decodeIdentifierTypes :: Int -> NonEmpty IdentifierType"
        , "decodeIdentifierTypes = \\case" <> encoding
        , "    _ -> " <> mkHaskellConstructorsList def
        , ""
        , "-- | Returns the 'IdentifierType's corresponding to a character."
        , code
        ]
        where
        toWord8 =
            assert (fromEnum (maxBound :: IdentifierType) < 0xff)
            (fromIntegral . fromEnum)
        (planes0To3, plane14) = splitPlanes
            "Cannot generate: genIdentifierTypeModule"
            (== defIdx)
            (reverse identifiersTypes)
        (encoding, identifiersTypes, defIdx) = mkIdentifiersTypes acc
        ShamochuCode{..} = genEnumBitmapShamochu
            "identifierTypes"
            False
            (NE.singleton 3)
            [5]
            toWord8
            (defIdx, BB.intDec (fromEnum defIdx))
            (defIdx, BB.intDec (fromEnum defIdx))
            planes0To3
            plane14
