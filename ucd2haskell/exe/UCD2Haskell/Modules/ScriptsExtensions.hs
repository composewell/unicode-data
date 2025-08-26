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

import Control.Arrow (Arrow (..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Semigroup (Arg (..))
import qualified Data.Set as Set
import Data.Word (Word16)
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Defaults as Defaults
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop

import UCD2Haskell.Common (Fold (..), mkHaskellConstructor)
import UCD2Haskell.Generator (
    FileRecipe (..),
    ShamochuCode (..),
    apacheLicense,
    enumMapToAddrLiteral8,
    genEnumBitmapShamochu,
    mkImports',
    splitPlanes,
    unlinesBB,
    (<+>),
 )

recipe :: PropertyValuesAliases -> ScriptExtensions -> FileRecipe Prop.Entry
recipe aliases extensions = ModuleRecipe
    "Unicode.Internal.Char.ScriptExtensions"
    (\m -> genScriptExtensionsModule m aliases extensions)

type PropertyValuesAliases = Map.Map BS.ShortByteString (NE.NonEmpty BS.ShortByteString)

defaultScriptAbbr :: BS.ShortByteString
defaultScriptAbbr = "Zzzz"

data Acc = Acc
    { usedScripts :: !(Set.Set BS.ShortByteString)
    , usedExts :: !(Set.Set (NE.NonEmpty BS.ShortByteString))
    , charsExts :: !(Map.Map Char (NE.NonEmpty BS.ShortByteString)) }

genScriptExtensionsModule ::
    BB.Builder ->
    PropertyValuesAliases ->
    ScriptExtensions ->
    Fold Prop.Entry BB.Builder
genScriptExtensionsModule moduleName aliases extensions = Fold step initial done
    where
    -- [NOTE] We rely on all the scripts having a short form

    initial = Acc
        { usedScripts = Set.singleton Defaults.defaultScript
        , usedExts = mempty
        , charsExts = mempty }

    -- Map: script → short form
    getScriptAbbr :: BS.ShortByteString -> BS.ShortByteString
    getScriptAbbr = maybe (error "script not found") NE.head . (aliases Map.!?)

    step :: Acc -> Prop.Entry -> Acc
    step acc (Prop.Entry range script) = case range of
        U.SingleChar c    -> addChar script c acc
        U.CharRange c1 c2 -> foldr (addChar script) acc [c1..c2]

    addChar
        :: BS.ShortByteString -- script
        -> Char               -- processed char
        -> Acc
        -> Acc
    addChar script c Acc{..} = case Map.lookup c extensions of
        -- Char has explicit extensions
        Just exts -> Acc
            { usedScripts = Set.insert script usedScripts
            , usedExts = Set.insert exts usedExts
            , charsExts = Map.insert c exts charsExts }
        -- Char has no explicit extensions: use its script
        Nothing   -> Acc
            { usedScripts = Set.insert script usedScripts
            , usedExts = Set.insert exts usedExts
            , charsExts = Map.insert c exts charsExts }
            where exts = getScriptAbbr script NE.:| []

    done Acc{..} = unlinesBB
        [ apacheLicense 2022 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(scriptExtensions)"
        , "where"
        , ""
        , mkImports' "Scripts" imports'
        , "-- | Script extensions of a character."
        , "--"
        , "-- Returns a pair:"
        , "--"
        , "-- * If first value is negative or zero, then its absolute value is a single script,"
        , "--   encoded by its index."
        , "-- * Else the first element is the length and the second is the list of scripts,"
        , "--   encoded by their index."
        , "--"
        , "-- @since 0.1.0"
        -- NOTE: we could use Unboxed sums once we drop support for GHC 8.0
        , "scriptExtensions :: Char -> (# Int#, Addr# #)"
        , "scriptExtensions c = case encodedScriptExtensions c of"
            <> mkDecodeScriptExtensions encodeExtensions encodeAbbr
                    (usedExts Set.\\ singleScriptExtensionsSet)
        , "    s    -> (# negateInt# s, nullAddr# #)"
        , ""
        , code
        ]
        where
        -- List ordered by Haskell constructors
        scripts
            = fmap (\(Arg _ a) -> a)
            . Set.toAscList
            . Set.map (\s -> Arg (mkHaskellConstructor' s) s)
            $ usedScripts
        toWord16 :: Word16 -> Word16
        toWord16 = if fromEnum (Map.size encodedExtensions) < 0xffff
            then fromIntegral . fromEnum
            else error "Too many script extensions"

        mkHaskellConstructor' = B.toStrict . BB.toLazyByteString . mkHaskellConstructor
        encodedAbbr :: Map.Map BS.ShortByteString Word16
        encodedAbbr = Map.fromList (first getScriptAbbr <$> zip scripts [0..])
        encodeAbbr :: BS.ShortByteString -> Word16
        encodeAbbr = (encodedAbbr Map.!)

        singleScriptExtensions = pure . getScriptAbbr <$> scripts
        singleScriptExtensionsSet = Set.fromList singleScriptExtensions
        multiScriptExtensions :: Set.Set (NE.NonEmpty BS.ShortByteString)
        multiScriptExtensions = Set.fromList (Map.elems extensions)
                                Set.\\ singleScriptExtensionsSet
        -- Encode single script as their script value
        extensionsList = singleScriptExtensions
                      <> Set.toList multiScriptExtensions

        encodedExtensions :: Map.Map (NE.NonEmpty BS.ShortByteString) Word16
        encodedExtensions = let len = length extensionsList in if len < 0xffff
            then Map.fromList (zip extensionsList [0..])
            else error ("Too many script extensions: " <> show len)

        encodeExtensions = (encodedExtensions Map.!)

        def = encodeExtensions (NE.singleton defaultScriptAbbr)
        scriptExtensions = mkScriptExtensions def (Map.map encodeExtensions charsExts)
        -- [TODO] simplify
        (planes0To3, plane14) = splitPlanes
            "Cannot generate: genScriptExtensionsModule"
            (== def)
            scriptExtensions
        ShamochuCode{..} = genEnumBitmapShamochu
            "encodedScriptExtensions"
            True
            (NE.singleton 3)
            [5]
            toWord16
            (def, BB.intDec (fromEnum def))
            (def, BB.intDec (fromEnum def))
            planes0To3
            plane14
        imports' = imports <+> Map.singleton
            "GHC.Exts"
            (Set.fromList ["Addr#", "Int(..)", "nullAddr#", "negateInt#"])

    mkDecodeScriptExtensions
        :: (NE.NonEmpty BS.ShortByteString -> Word16)
        -> (BS.ShortByteString -> Word16)
        -> Set.Set (NE.NonEmpty BS.ShortByteString)
        -> BB.Builder
    mkDecodeScriptExtensions encodeExtensions encodeAbbr
        = mkDecodeScriptExtensions'
        . Set.map (\exts -> Arg (encodeExtensions exts)
                                (NE.sort (encodeAbbr <$> exts)))

    mkDecodeScriptExtensions' = foldMap $ \(Arg v exts) -> mconcat
        [ "\n    "
        , BB.word16Dec v
        , "# -> (# "
        , BB.intDec (length exts)
        , "#, \""
        , enumMapToAddrLiteral8 0 0xff (NE.toList exts) "\"# #)" ]

    mkScriptExtensions def
        = reverse
        . snd
        . Map.foldlWithKey (addCharExt def) ('\0', mempty)
    addCharExt def (expected, acc) c v = if expected < c
        then addCharExt def (succ expected, def : acc) c v
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

