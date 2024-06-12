-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.ScriptsExtensions
    ( recipe
    , parseScriptExtensions
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
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

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense, genEnumBitmap, splitPlanes)
import UCD2Haskell.Common (Fold (..), mkHaskellConstructor)

recipe :: PropertyValuesAliases -> ScriptExtensions -> FileRecipe Prop.Entry
recipe aliases extensions = ModuleRecipe
    "Unicode.Internal.Char.ScriptExtensions"
    (\m -> genScriptExtensionsModule m aliases extensions)

type PropertyValuesAliases = Map.Map BS.ShortByteString (NE.NonEmpty BS.ShortByteString)

genScriptExtensionsModule :: BB.Builder -> PropertyValuesAliases -> ScriptExtensions -> Fold Prop.Entry BB.Builder
genScriptExtensionsModule moduleName aliases extensions = Fold step mempty done
    where
    -- [NOTE] We rely on all the scripts having a short form

    -- Map: abbreviation -> script
    scripts = Map.foldlWithKey'
        (\acc s as -> Map.insert (NE.head as) s acc)
        mempty
        aliases

    -- Map: script → short form
    getScriptAbbr :: BS.ShortByteString -> BS.ShortByteString
    getScriptAbbr = maybe (error "script not found") NE.head . (aliases Map.!?)

    -- All possible values: extensions + scripts
    extensionsSet :: Set.Set (NE.NonEmpty BS.ShortByteString)
    extensionsSet = Set.fromList (Map.elems extensions)
                  <> Set.map pure (Map.keysSet scripts)
    extensionsList = L.sortBy
        (compare `on` fmap (scripts Map.!))
        (Set.toList extensionsSet)

    encodeExtensions :: NE.NonEmpty BS.ShortByteString -> Int
    encodeExtensions e = fromMaybe
        (error ("extension not found: " <> show e))
        (L.elemIndex e extensionsList)

    encodedExtensions :: Map.Map (NE.NonEmpty BS.ShortByteString) Int
    encodedExtensions =
        let l = length extensionsSet
        in if length extensionsSet > 0xff
            then error ("Too many script extensions: " <> show l)
            else Map.fromSet encodeExtensions extensionsSet

    step
        :: (Set.Set (NE.NonEmpty BS.ShortByteString), Map.Map Char Int) -- used exts, encoded char exts
        -> Prop.Entry
        -> (Set.Set (NE.NonEmpty BS.ShortByteString), Map.Map Char Int)
    step acc (Prop.Entry range script) = case range of
        U.SingleChar c    -> addChar script c acc
        U.CharRange c1 c2 -> foldr (addChar script) acc [c1..c2]

    addChar
        :: BS.ShortByteString -- script
        -> Char   -- processed char
        -> (Set.Set (NE.NonEmpty BS.ShortByteString), Map.Map Char Int)
        -> (Set.Set (NE.NonEmpty BS.ShortByteString), Map.Map Char Int)
    addChar script c (extsAcc, charAcc) = case Map.lookup c extensions of
        -- Char has explicit extensions
        Just exts -> ( Set.insert exts extsAcc
                     , Map.insert c (encodedExtensions Map.! exts) charAcc)
        -- Char has no explicit extensions: use its script
        Nothing   ->
            let exts = getScriptAbbr script NE.:| []
            in ( Set.insert exts extsAcc
               , Map.insert c (encodedExtensions Map.! exts) charAcc)

    done (usedExts, exts) = unlinesBB
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE OverloadedLists #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(scriptExtensions, decodeScriptExtensions)"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Data.List.NonEmpty (NonEmpty)"
        , "import Data.Word (Word8)"
        , "import GHC.Exts (Ptr(..))"
        , "import Unicode.Internal.Char.Scripts (Script(..))"
        , "import Unicode.Internal.Bits (lookupWord8AsInt)"
        , ""
        , "-- | Useful to decode the output of 'scriptExtensions'."
        , "decodeScriptExtensions :: Int -> NonEmpty Script"
        , "decodeScriptExtensions = \\case" <> mkDecodeScriptExtensions usedExts
        , "    _   -> [" <> mkHaskellConstructor Defaults.defaultScript <> "]"
        , ""
        , "-- | Script extensions of a character."
        , "--"
        , "-- @since 0.1.0"
        , genEnumBitmap
            "scriptExtensions"
            (def, BB.intDec (fromEnum def))
            (def, BB.intDec (fromEnum def))
            planes0To3
            plane14
        ]
        where
        scriptExtensions = mkScriptExtensions exts
        -- [TODO] simplify
        (planes0To3, plane14) = splitPlanes
            "Cannot generate: genScriptExtensionsModule"
            (== def)
            scriptExtensions

    mkDecodeScriptExtensions :: Set.Set (NE.NonEmpty BS.ShortByteString) -> BB.Builder
    mkDecodeScriptExtensions
        = mkDecodeScriptExtensions'
        . Set.map (\exts -> (encodedExtensions Map.! exts, exts))
    mkDecodeScriptExtensions' = foldMap $ \(v, exts) -> mconcat
        [ "\n    "
        , BB.intDec v
        , " -> ["
        , sconcat (NE.intersperse ", " (mkScript <$> exts))
        , "]"
        ]
    mkScript :: BS.ShortByteString -> BB.Builder
    mkScript = mkHaskellConstructor . (scripts Map.!)

    def :: Int
    def = encodedExtensions Map.! (getScriptAbbr Defaults.defaultScript NE.:| [])

    mkScriptExtensions
        = reverse
        . snd
        . Map.foldlWithKey addCharExt ('\0', mempty)
    addCharExt (expected, acc) c v = if expected < c
        then addCharExt (succ expected, def : acc) c v
        else (succ c, v : acc)

type ScriptExtensions = Map.Map Char (NE.NonEmpty BS.ShortByteString)

parseScriptExtensions :: B.ByteString -> ScriptExtensions
parseScriptExtensions = foldr addExt mempty . Prop.parse
    where
        addExt Prop.Entry{..} = case range of
            U.SingleChar c -> Map.insertWith (<>) c values
            U.CharRange{..} -> \acc -> foldl'
                (\a c -> Map.insertWith (<>) c values a)
                acc
                [start..end]
            where
                values = NE.fromList (U.parseList value)

