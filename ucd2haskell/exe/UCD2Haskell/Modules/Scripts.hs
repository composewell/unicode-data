-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.Scripts (recipe, parseScriptAliases) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
import Data.Char (ord)
import Data.Bits (Bits(..))
import Data.Foldable (Foldable(..))
import Data.Function (on)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Defaults as Defaults
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop
import qualified Unicode.CharacterDatabase.Parser.PropertyValueAliases as PVA

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense, word32ToWord8s, genEnumBitmap, splitPlanes)
import UCD2Haskell.Common (Fold (..), mkHaskellConstructor)

recipe :: PropertyValuesAliases -> FileRecipe Prop.Entry
recipe aliases = ModuleRecipe
    "Unicode.Internal.Char.Scripts"
    (`genScriptsModule` aliases)

type PropertyValuesAliases = Map.Map BS.ShortByteString (NE.NonEmpty BS.ShortByteString)

genScriptsModule :: BB.Builder -> PropertyValuesAliases -> Fold Prop.Entry BB.Builder
genScriptsModule moduleName aliases = Fold step mempty done
    -- done <$> Fold.foldl' addRange mempty
    where

    done ranges =
        let scripts = Set.toList
                        (foldr addScript (Set.singleton Defaults.defaultScript) ranges)
        in unlinesBB
            [ apacheLicense 2022 moduleName
            , "{-# OPTIONS_HADDOCK hide #-}"
            , ""
            , "module " <> moduleName
            , "(Script(..), script, scriptDefinition)"
            , "where"
            , ""
            , "import Data.Char (ord)"
            , "import Data.Int (Int32)"
            , "import Data.Ix (Ix)"
            , "import Data.Word (Word8)"
            , "import GHC.Exts (Ptr(..))"
            , "import Unicode.Internal.Bits (lookupIntN)"
            , ""
            , "-- | Unicode [script](https://www.unicode.org/reports/tr24/)."
            , "--"
            , "-- The constructors descriptions are the original Unicode values"
            , "-- (short and long forms)."
            , "--"
            , "-- There is a total of " <> BB.intDec (length scripts) <> " scripts."
            , "--"
            , "-- @since 0.1.0"
            , "data Script"
            , "  = " <> mkScripts scripts
            , "  deriving (Enum, Bounded, Eq, Ord, Ix, Show)"
            , ""
            , "-- | Script definition: list of corresponding characters."
            , "--"
            , "-- @since 0.1.0"
            , "scriptDefinition :: Script -> (Ptr Int32, Int)"
            , "scriptDefinition b = case b of"
            , mkScriptDefinitions ranges
            , "-- | Script of a character."
            , "--"
            , "-- @since 0.1.0"
            , if length scripts <= 0xff
                then mkCharScripts scripts ranges
                else error "Cannot encode scripts"
            , ""
            ]

    step :: [Prop.Entry] -> Prop.Entry -> [Prop.Entry]
    step acc l@(Prop.Entry r script) = case acc of
        Prop.Entry r' script':acc' -> if script == script'
            then case combineRanges r r' of
                Left  r'' -> Prop.Entry r'' script : acc
                Right r'' -> Prop.Entry r'' script : acc'
            else l : acc
        _ -> [l]

    combineRanges :: U.CodePointRange -> U.CodePointRange -> Either U.CodePointRange U.CodePointRange
    combineRanges r = case r of
        U.SingleChar c1 -> \case
            U.SingleChar c2 -> if c1 == succ c2
                then Right (U.CharRange c2 c1)
                else Left r
            U.CharRange c2 c3 -> if c1 == succ c3
                then Right (U.CharRange c2 c1)
                else Left r
        U.CharRange c1 c2 -> \case
            U.SingleChar c3 -> if c1 == succ c3
                then Right (U.CharRange c3 c2)
                else Left r
            U.CharRange c3 c4 -> if c1 == succ c4
                then Right (U.CharRange c3 c2)
                else Left r

    addScript :: Prop.Entry -> Set.Set BS.ShortByteString -> Set.Set BS.ShortByteString
    addScript (Prop.Entry _ script) = Set.insert script

    mkScripts :: [BS.ShortByteString] -> BB.Builder
    mkScripts
        = mconcat
        . L.intersperse "\n  | "
        . fmap (\script -> mconcat
            [ mkHaskellConstructor script
            , " -- ^ "
            , case Map.lookup script aliases of
                Just as -> mkAliases as
                Nothing -> error ("No abbreviation for script: " <> show script)
            , ": @"
            , BB.shortByteString script
            , "@"
            ])

    mkAliases
        = sconcat
        . NE.intersperse ", "
        . fmap (\abbr -> mconcat ["@", BB.shortByteString abbr, "@"])

    mkScriptDefinitions :: [Prop.Entry] -> BB.Builder
    mkScriptDefinitions
        = foldMap mkScriptDefinition
        . NE.groupBy ((==) `on` Prop.value)
        . NE.fromList
        . reverse
        . addUnknownRanges

    addUnknownRanges :: [Prop.Entry] -> [Prop.Entry]
    addUnknownRanges ls =
        let addUnknown (acc, expected) (c, _) = case mkMissingRange expected c of
                Just r -> (,succ c) $ case acc of
                    r':acc' -> either (:acc) (:acc') (combineRanges r r')
                    _       -> [r]
                Nothing -> (acc, succ expected)
            addRest (acc@(r':acc'), expected) =
                let r = U.CharRange expected maxBound
                in either (:acc) (:acc') (combineRanges r r')
            addRest _ = error "impossible"
            unknown = fmap (`Prop.Entry` Defaults.defaultScript) . addRest $ foldl'
                addUnknown
                (mempty, '\0')
                (L.sort (foldMap (rangeToCharScripts id) ls))
        in unknown <> ls

    mkMissingRange :: Char -> Char -> Maybe U.CodePointRange
    mkMissingRange expected c
        | c == expected      = Nothing
        | c == succ expected = Just (U.SingleChar expected)
        | otherwise          = Just (U.CharRange expected (pred c))

    mkScriptDefinition :: NE.NonEmpty Prop.Entry -> BB.Builder
    mkScriptDefinition ranges = mconcat
        [ "  "
        , mkHaskellConstructor (Prop.value (NE.head ranges))
        , " -> (Ptr \""
        , foldMap encodeRange ranges
        , "\"#, "
        , BB.wordDec
            (foldr
                (\(Prop.Entry r _) -> case r of
                    U.SingleChar{} -> (+1)
                    U.CharRange{} -> (+2))
                0
                ranges)
        , ")\n"
        ]

    -- Encoding:
    -- • A single char is encoded as an LE Int32.
    -- • A range is encoded as two LE Int32 (first is lower bound, second is
    --   upper bound), which correspond to the codepoints with the 32th bit set.
    encodeRange :: Prop.Entry -> BB.Builder
    encodeRange (Prop.Entry r _) = case r of
        U.SingleChar  c -> encodeBytes (fromIntegral (ord c))
        U.CharRange l u -> encodeBytes (setBit (fromIntegral (ord l)) 31)
                     <> encodeBytes (setBit (fromIntegral (ord u)) 31)
    encodeBytes = foldr addByte "" . word32ToWord8s
    addByte n acc = BB.char7 '\\' <> BB.word8Dec n <> acc

    mkCharScripts :: [BS.ShortByteString] -> [Prop.Entry] -> BB.Builder
    mkCharScripts scripts scriptsRanges =
        let charScripts = L.sort (foldMap (rangeToCharScripts getScript) scriptsRanges)
            charScripts' = reverse (fst (foldl' addMissing (mempty, '\0') charScripts))
            addMissing (acc, expected) x@(c, script) = if expected < c
                then addMissing (def:acc, succ expected) x
                else (script:acc, succ c)
            def = getScript Defaults.defaultScript
            getScript s = fromMaybe (error "script not found") (L.elemIndex s scripts)
            -- [TODO] simplify
            (planes0To3, plane14) = splitPlanes "Cannot generate: genScriptsModule" (== def) charScripts'
        in genEnumBitmap
            "script"
            (def, BB.intDec (fromEnum def))
            (def, BB.intDec (fromEnum def))
            planes0To3
            plane14

    rangeToCharScripts :: (BS.ShortByteString -> b) -> Prop.Entry -> [(Char, b)]
    rangeToCharScripts f (Prop.Entry r script) = case r of
        U.SingleChar cp -> [(cp, f script)]
        U.CharRange l u -> (, f script) <$> [l..u]

-- Map: script long form → short forms
parseScriptAliases :: B.ByteString -> PropertyValuesAliases
parseScriptAliases = foldr addScript mempty . PVA.parse
    where
    addScript PVA.Entry{..}
        | property /= "sc" = id
        | otherwise = Map.insert
            (PVA.longName value)
            (PVA.shortName value NE.:| PVA.aliases value)
