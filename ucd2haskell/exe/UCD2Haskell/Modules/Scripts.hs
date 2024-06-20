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
import Data.Bits (Bits (..))
import Data.Foldable (Foldable (..))
import Data.Function (on)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup (..))
import Control.Applicative (Alternative (..))
import Control.Exception (assert)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Defaults as Defaults
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop
import qualified Unicode.CharacterDatabase.Parser.PropertyValueAliases as PVA

import UCD2Haskell.Common (
    Fold (..),
    mkHaskellConstructor,
    showHexCodepointB,
    showPaddedHeXB,
 )
import UCD2Haskell.Generator (
    BitmapType (..),
    FileRecipe (..),
    apacheLicense,
    generateShamochuBitmaps,
    toLookupBitMapName,
    unlinesBB,
    word32ToWord8s,
 )

recipe :: PropertyValuesAliases -> FileRecipe Prop.Entry
recipe aliases = ModuleRecipe
    "Unicode.Internal.Char.Scripts"
    (`genScriptsModule` aliases)

type PropertyValuesAliases = Map.Map BS.ShortByteString (NE.NonEmpty BS.ShortByteString)

genScriptsModule :: BB.Builder -> PropertyValuesAliases -> Fold Prop.Entry BB.Builder
genScriptsModule moduleName aliases = Fold step mempty done
    where

    done ranges =
        let scripts = Set.toList
                        (foldr addScript (Set.singleton Defaults.defaultScript) ranges)
        in unlinesBB
            [ "{-# LANGUAGE PatternSynonyms #-}"
            , "{-# OPTIONS_HADDOCK hide #-}"
            , ""
            , apacheLicense 2022 moduleName
            , ""
            , "module " <> moduleName
            , "  ( Script(..)"
            , "  , scriptShortName"
            , "  , script"
            , "  , scriptDefinition"
            , "  , pattern ScriptCharMask"
            , "  , pattern ScriptCharMaskComplement )"
            , "where"
            , ""
            , "import Data.Char (ord)"
            , "import Data.Int (Int8)"
            , "import Data.Ix (Ix)"
            , "import Data.Word (Word16)"
            , "import GHC.Exts (Addr#, Int#, Int(..), Ptr(..), nullAddr#, andI#, iShiftL#, iShiftRL#, (+#), (-#))"
            , "import Unicode.Internal.Bits.Scripts (lookupWord8AsInt#, lookupWord16AsInt#)"
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
            , "-- | Returns the 4-letter ISO 15924 script code as a NUL-terminated CString."
            , "--"
            , "-- @since 0.3.0"
            , "scriptShortName :: Script -> Addr#"
            , "scriptShortName = \\case" <> mkScriptShortNames ranges
            , ""
            , "-- | Used to detect single character in ranges."
            , "pattern ScriptCharMask :: Int#"
            , "pattern ScriptCharMask = 0x800000# -- 1 << 23"
            , ""
            , "-- | Used to extract single character in ranges."
            , "pattern ScriptCharMaskComplement :: Int#"
            , "pattern ScriptCharMaskComplement = 0x7fffff# -- 1 << 23 ^ 0xffffff"
            , ""
            , "-- | Script definition: list of corresponding characters."
            , "--"
            , "-- Returned tuple value:"
            , "--"
            , "-- 1. Minimum codepoint."
            , "-- 2. Maximum codepoint."
            , "-- 3. Bitmap of ranges, without the extrema."
            , "-- 4. Offset of last entry, in bytes."
            , "--"
            , "-- Encoding:"
            , ""
            , "-- * A single character is encoded as an LE Word32:"
            , "--   codepoint with the 24th bit set."
            , "-- * A range is encoded as two LE Word32: first is lower bound codepoint,"
            , "--   second is upper bound codepoint."
            , "--"
            , "-- @since 0.1.0"
            , "scriptDefinition :: Script -> (# Int#, Int#, Addr#, Int# #)"
            , "scriptDefinition = \\case"
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


    mkScriptShortNames :: [Prop.Entry] -> BB.Builder
    mkScriptShortNames ranges = Map.foldMapWithKey
         (\script abbr -> if script `elem` scriptsSet
            then mconcat
                [ "\n    "
                , mkHaskellConstructor script
                -- [NOTE] ASCII string
                , " -> \""
                , BB.shortByteString (NE.head abbr)
                , "\\0\"#" ]
            else mempty)
          aliases
        where
        scriptsSet = Set.insert Defaults.defaultScript
                   $ foldr
                        (\(Prop.Entry{value=v}) -> Set.insert v)
                        mempty
                        ranges

    mkScriptDefinitions :: [Prop.Entry] -> BB.Builder
    mkScriptDefinitions
        = foldMap mkScriptDefinition
        . NE.groupBy ((==) `on` Prop.value)
        . NE.fromList
        . L.sortBy (compare `on` (\(Prop.Entry r v) -> (v, r)))
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
    mkScriptDefinition = \case
        Prop.Entry r script NE.:| [] -> mconcat
            [ "    "
            , mkHaskellConstructor script
            , " -> (# 0x"
            , showHexCodepointB (U.start r)
            , "#, 0x"
            , showHexCodepointB (getEnd r)
            , "#, nullAddr#, 0# #)\n"
            ]
            where
            getEnd = \case
                U.SingleChar c -> c
                U.CharRange _ c -> c
        ranges@(Prop.Entry{range=firstRange, value=script} NE.:| _) -> mconcat
            [ "    "
            , mkHaskellConstructor script
            , " -> (# 0x"
            , showHexCodepointB lower
            , "#, 0x"
            , showHexCodepointB upper
            , "#, \""
            , mconcat encodedRanges
            , "\"#, "
            , BB.intDec ((count - 1) * 4)
            , "# #)\n"
            ]
            where
            getRange Prop.Entry{range=r} = r
            firstRange' = case firstRange of
                U.SingleChar{}         -> []
                U.CharRange c1 c2 -> if succ c1 == c2
                    then [U.SingleChar c2]
                    else [U.CharRange (succ c1) c2]
            lastRange  = getRange (NE.last ranges)
            lastRange' = case lastRange of
                U.SingleChar{}    -> []
                U.CharRange c1 c2 -> if c1 == pred c2
                    then [U.SingleChar c1]
                    else [U.CharRange c1 (pred c2)]
            lower = U.start firstRange
            upper = case lastRange of
                U.SingleChar c -> c
                U.CharRange _ c -> c
            ranges' = case firstRange' <> fmap getRange (drop 1 (NE.init ranges)) <> lastRange' of
                [] -> error ("mkScriptDefinition: empty literal for " <> show script)
                rs -> rs
            (encodedRanges, count) = foldr go (mempty, 0) ranges'
            go r (bs, c) = case encodeRange r of
                (b, n) -> (b : bs, c + n)

    -- • A single character is encoded as an LE Word32:
    --   codepoint with the 24th bit set.
    -- • A range is encoded as two LE Word32: first is lower bound codepoint,
    --   second is upper bound codepoint.
    encodeRange :: U.CodePointRange -> (BB.Builder, Int)
    encodeRange = \case
        U.SingleChar  c      -> (encodeBytes (setBit (fromIntegral (ord c)) 23), 1)
        U.CharRange l u -> (encodeBytes (fromIntegral (ord l))
                      <> encodeBytes (fromIntegral (ord u)), 2)
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
            isDef = (== def)
            getScript s = fromMaybe (error "script not found") (L.elemIndex s scripts)
            -- In order to reduce the size of the bitmap,
            -- we divide the characters by planes:
            -- • Planes 0 to 1: bitmap lookup
            -- • Planes 2 to 3: fast ranges check (“Han” or “Unknown” scripts)
            -- • Planes 4 to 13: “Unknown” script
            -- • Plane 14: fast ranges check
            -- • Planes 15-16: “Unknown” script
            planes0To1 = L.dropWhileEnd isDef (take 0x20000 charScripts')
            planes2To16 = L.dropWhileEnd isDef (drop 0x20000 charScripts')
            planes2To3 = L.dropWhileEnd isDef (take (0x40000 - 0x20000) planes2To16)
            plane14 = L.dropWhileEnd isDef (drop (0xE0000 - 0x20000) planes2To16)
            boundPlanes0To1 = length planes0To1
            otherPlanes = zip planes2To3 ['\x20000'..]
                       <> zip plane14    ['\xE0000'..]
            toWord8 =
                assert (fromEnum (length scripts) < 0xff)
                (fromIntegral . fromEnum)
            bitmap0To1 = "scriptPlanes0To1"
        in mconcat
            [ "{-# INLINE script #-}\n"
            , "script :: Char -> Int#\n"
            , "script c\n"
            , "    -- Planes 0-1\n"
            , "    | cp < 0x", showPaddedHeXB boundPlanes0To1
            , " = ", toLookupBitMapName bitmap0To1, " cp#\n"
            , mkScriptsBounds def (scripts !!) otherPlanes
            , "    -- Default: ", BB.shortByteString Defaults.defaultScript, "\n"
            , "    | otherwise = ", BB.intDec def, "#\n"
            , "    where\n"
            , "    !cp@(I# cp#) = ord c\n"
            , "\n"
            , generateShamochuBitmaps
                bitmap0To1
                True
                ByteMap
                (NE.singleton 3)
                [5]
                toWord8
                planes0To1
            ]

    mkScriptsBounds :: Int -> (Int -> BS.ShortByteString) -> [(Int,Char)] -> BB.Builder
    mkScriptsBounds def getScriptName
        = foldMap (mkScriptBound getScriptName)
        . NE.groupBy ((==) `on` fst)
        . filter ((/= def) . fst)
        . foldr addScriptBound mempty

    mkScriptBound :: (Int -> BS.ShortByteString) -> NE.NonEmpty (Int, (Char, Char)) -> BB.Builder
    mkScriptBound getScriptName xs = mconcat
        [ "    -- "
        , mconcat $ if maxPlane /= minPlane
            then [ "Planes ", BB.intDec minPlane, "-", BB.intDec maxPlane, ": "]
            else [ "Plane ", BB.intDec minPlane, ": "]
        , BB.shortByteString (getScriptName script), "\n"
        , "    | cp <= 0x", showPaddedHeXB (ord (snd lastRange))
        , fromMaybe mempty $ mkBinarySearch [snd lastRange] " && " (snd <$> NE.toList xs)
        , " = ", BB.intDec script, "#\n"
        ]
        where
        script = fst (NE.head xs)
        firstRange = snd (NE.head xs)
        lastRange = snd (NE.last xs)
        minPlane = ord (fst firstRange) `shiftR` 16
        maxPlane = ord (snd lastRange) `shiftR` 16
        mkBinarySearch :: [Char] -> BB.Builder -> [(Char, Char)] -> Maybe BB.Builder
        mkBinarySearch bs prefix = \case
            [] -> Nothing
            [(l, u)] -> if l `elem` bs
                then if u `elem` bs
                    then Nothing
                    else Just (mconcat [ prefix, "cp <= 0x", showPaddedHeXB (ord u) ])
                else if u `elem` bs
                    then Just (mconcat [ prefix, "cp >= 0x", showPaddedHeXB (ord l) ])
                    else if l == u
                        then Just (mconcat [ prefix, "cp == 0x", showPaddedHeXB (ord l)])
                        else Just $ mconcat [ prefix
                             , "(cp >= 0x", showPaddedHeXB (ord l), " && "
                             , "cp <= 0x", showPaddedHeXB (ord u), ")" ]
            ys@(y : _) ->
                let !(l, l') = y
                    !lCP = ord l
                    !lCP' = ord l'
                    !u = snd (last ys)
                    !uCP = ord u
                    !b1 = max (lCP' + 1) (lCP + div (uCP - lCP) 2)
                    isLowerHalf (ord -> x, ord -> z) =
                        z < b1 || (x <= b1 && (b1 - x > z - b1))
                    (before, after, b2) = case L.partition isLowerHalf ys of
                        (b@(_:_), a@(a0:_)) -> (b, a, fst a0)
                        _ -> error (show (script, before, after))
                    !search1 = mkBinarySearch (b2:bs) " && " after
                    !search2 = mkBinarySearch bs      " || " before
                in case search1 <|> search2 of
                    Nothing -> Just (mconcat [ prefix, "cp >= 0x", showPaddedHeXB (ord b2) ])
                    _ -> Just . mconcat $
                        [ prefix
                        , "(cp >= 0x"
                        , showPaddedHeXB (ord b2)
                        , fromMaybe mempty search1
                        , fromMaybe mempty search2
                        , ")" ]

    addScriptBound
        :: (Int, Char)
        -> [(Int, (Char, Char))]
        -> [(Int, (Char, Char))]
    addScriptBound (s, c) = \case
        rs@((s', (_, u)) : rs') -> if s == s'
            then (s, (c, u)) : rs'
            else (s, (c, c)) : rs
        [] -> [(s, (c, c))]

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
