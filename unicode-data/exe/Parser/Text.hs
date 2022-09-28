{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Parser.Text
-- Copyright   : (c) 2021 Pierre Le Marre
--               (c) 2020 Composewell Technologies and Contributors
--               (c) 2016-2017 Harendra Kumar
--               (c) 2014-2015 Antonio Nikishaev
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental

-- The original Unicode database parser was taken from
-- https://github.com/composewell/unicode-transforms but was completely
-- rewritten from scratch to parse from UCD text files instead of XML, only
-- some types remain the same. That code in turn was originally taken from
-- https://github.com/llelf/prose (Antonio Nikishaev) and heavily modified by
-- Harendra Kumar.
--
module Parser.Text
    ( genCoreModules
    , genNamesModules
    , genScriptsModules
    , genSecurityModules
    ) where

import Control.Applicative (Alternative(..))
import Control.Arrow ((&&&))
import Control.Exception (assert, catch, IOException)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bifunctor (Bifunctor(..))
import Data.Bits (Bits(..))
import Data.Char (chr, ord, isAlphaNum, isAscii, isSpace, toUpper)
import Data.Foldable (foldl')
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List
    (dropWhileEnd, elemIndex, groupBy, intersperse, sort, sortBy, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Word (Word8, Word32)
import Numeric (showHex)
import Streamly.Data.Fold (Fold)
import Streamly.Prelude (IsStream, SerialT)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Foreign as Foreign
import qualified GHC.IO.Encoding as Encoding
import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.FileSystem.Handle as Handle
import qualified System.IO as Sys
import qualified Streamly.Unicode.Stream as Unicode

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data GeneralCategory =
    Lu|Ll|Lt|             --LC
    Lm|Lo|                --L
    Mn|Mc|Me|             --M
    Nd|Nl|No|             --N
    Pc|Pd|Ps|Pe|Pi|Pf|Po| --P
    Sm|Sc|Sk|So|          --S
    Zs|Zl|Zp|             --Z
    Cc|Cf|Cs|Co|Cn        --C
    deriving (Show, Bounded, Enum, Read)

data DecompType =
       DTCanonical | DTCompat  | DTFont
     | DTNoBreak   | DTInitial | DTMedial   | DTFinal
     | DTIsolated  | DTCircle  | DTSuper    | DTSub
     | DTVertical  | DTWide    | DTNarrow
     | DTSmall     | DTSquare  | DTFraction
    deriving (Show, Eq)

data Decomp = DCSelf | DC [Char] deriving (Show, Eq)

data DType = Canonical | Kompat

data DetailedChar =
    DetailedChar
        { _char :: Char
        , _name :: String
        , _generalCategory :: GeneralCategory
        , _combiningClass :: Int
        , _decompositionType :: Maybe DecompType
        , _decomposition :: Decomp
        , _simpleUpperCaseMapping :: Maybe Char
        , _simpleLowerCaseMapping :: Maybe Char
        , _simpleTitleCaseMapping :: Maybe Char
        }
    deriving (Show)

type CharRange = Either Char (Char, Char)
data CharRangeStream
    = SingleChar !Char
    | CharRange !Char !Char
    | Stop

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

apacheLicense
    :: Word   -- ^ Copyright year
    -> String -- ^ Module name
    -> String
apacheLicense year modName =
    unlines
        [ "-- |"
        , "-- Module      : " <> modName
        , "-- Copyright   : (c) "
            <> show year
            <> " Composewell Technologies and Contributors"
        , "-- License     : Apache-2.0"
        , "-- Maintainer  : streamly@composewell.com"
        , "-- Stability   : experimental"
        ]

readCodePoint :: String -> Char
readCodePoint = chr . read . ("0x"++)

readCodePointM :: String -> Maybe Char
readCodePointM "" = Nothing
readCodePointM u  = Just (readCodePoint u)

parseCodePointRange :: String -> Either Char (Char, Char)
parseCodePointRange
    = (\(c1, c2) -> maybe (Left c1) (Right . (c1,)) c2)
    . bimap readCodePoint (readCodePointM . drop 2)
    . span (/= '.')

showPaddedHex :: Int -> String
showPaddedHex cp =
    let hex = showHex cp mempty
        padding = 4 - length hex
    in replicate padding '0' <> hex

showPaddedHeX :: Int -> String
showPaddedHeX = fmap toUpper . showPaddedHex

showHexCodepoint :: Char -> String
showHexCodepoint = showPaddedHex . ord

genSignature :: String -> String
genSignature = (<> " :: Char -> Bool")

-- | Check that var is between minimum and maximum of orderList
genRangeCheck :: String -> [Int] -> String
genRangeCheck var ordList =
    var
        <> " >= "
        <> show (minimum ordList)
        <> " && " <> var <> " <= " <> show (maximum ordList)

genBitmap :: String -> [Int] -> String
genBitmap funcName ordList =
    unlines
        [ "{-# INLINE " <> funcName <> " #-}"
        , genSignature funcName
        , funcName <> " = \\c -> let n = ord c in "
              <> genRangeCheck "n" ordList <> " && lookupBit64 bitmap# n"
        , "  where"
        , "    bitmap# = \"" <> bitMapToAddrLiteral (positionsToBitMap ordList) "\"#"
        ]

positionsToBitMap :: [Int] -> [Bool]
positionsToBitMap = go 0

    where

    go _ [] = []
    go i xxs@(x:xs)
        | i < x = False : go (i + 1) xxs
        | otherwise = True : go (i + 1) xs

bitMapToAddrLiteral
  :: [Bool]
  -- ^ Values to encode
  -> String
  -- ^ String to append
  -> String
bitMapToAddrLiteral bs cs = foldr encode cs (unfoldr mkChunks bs)

    where

    mkChunks :: [a] -> Maybe ([a], [a])
    mkChunks [] = Nothing
    mkChunks xs = Just $ splitAt 8 xs

    encode :: [Bool] -> String -> String
    encode chunk acc = '\\' : shows (toByte (padTo8 chunk)) acc

    padTo8 :: [Bool] -> [Bool]
    padTo8 xs
        | length xs >= 8 = xs
        | otherwise = xs ++ replicate (8 - length xs) False

    toByte :: [Bool] -> Int
    toByte xs = sum $ map (\i -> if xs !! i then 1 `shiftL` i else 0) [0..7]

genEnumBitmap
  :: forall a. (Bounded a, Enum a, Show a)
  => String
  -- ^ Function name
  -> a
  -- ^ Default value
  -> [a]
  -- ^ List of values to encode
  -> String
genEnumBitmap funcName def as = unlines
    [ "{-# INLINE " <> funcName <> " #-}"
    , funcName <> " :: Char -> Int"
    , funcName <> " c = let n = ord c in if n >= "
               <> show (length as)
               <> " then "
               <> show (fromEnum def)
               <> " else lookupIntN bitmap# n"
    , "  where"
    , "    bitmap# = \"" <> enumMapToAddrLiteral as "\"#"
    ]

{-| Encode a list of values as a byte map, using their 'Enum' instance.

__Note:__ 'Enum' instance must respect the following:

* @fromEnum minBound >= 0x00@
* @fromEnum maxBound <= 0xff@
-}
enumMapToAddrLiteral
  :: forall a. (Bounded a, Enum a, Show a)
  => [a]
  -- ^ Values to encode
  -> String
  -- ^ String to append
  -> String
enumMapToAddrLiteral xs cs = foldr go cs xs

    where

    go :: a -> String -> String
    go x acc = '\\' : shows (toWord8 x) acc

    toWord8 :: a -> Word8
    toWord8 a = let w = fromEnum a in if 0 <= w && w <= 0xff
        then fromIntegral w
        else error $ "Cannot convert to Word8: " <> show a

-- Encode Word32 to [Word8] little endian
word32ToWord8s :: Word32 -> [Word8]
word32ToWord8s n = (\k -> fromIntegral ((n `shiftR` k) .&. 0xff)) <$> [0,8..24]

-- This bit of code is duplicated but this duplication allows us to reduce 2
-- dependencies on the executable.

jamoLCount :: Int
jamoLCount = 19

jamoVCount :: Int
jamoVCount = 21

jamoTCount :: Int
jamoTCount = 28

hangulFirst :: Int
hangulFirst = 0xac00

hangulLast :: Int
hangulLast = hangulFirst + jamoLCount * jamoVCount * jamoTCount - 1

isHangul :: Char -> Bool
isHangul c = n >= hangulFirst && n <= hangulLast
    where n = ord c

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

-- Make a valid Haskell constructor (in CamelCase) from an identifier.
mkHaskellConstructor :: String -> String
mkHaskellConstructor = reverse . fst . foldl' convert (mempty, True)
    where

    convert (acc, newWord) = \case
        -- Skip the following and start a new word
        ' ' -> (acc, True)
        '-' -> (acc, True)
        '_' -> (acc, True)
        -- Letter or number
        c   -> if isAscii c && isAlphaNum c
            then ( if newWord then toUpper c : acc else c : acc
                 , False)
            else error ("Unsupported character: " <> show c)

genBlocksModule
    :: Monad m
    => String
    -> Fold m BlockLine String
genBlocksModule moduleName = done <$> Fold.foldl' step initial
    where

    done (blocks, defs, ranges) = let ranges' = reverse ranges in unlines
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE CPP, MultiWayIf #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(Block(..), BlockDefinition(..), block, blockDefinition)"
        , "where"
        , ""
        , "import Data.Ix (Ix)"
        , "import GHC.Exts"
        , ""
        , "-- | Unicode [block](https://www.unicode.org/glossary/#block)."
        , "--"
        , "-- There is a total of " <> show (length blocks) <> " blocks."
        , "--"
        , "-- @since 0.3.1"
        , "data Block"
        , "    = " <> mconcat (intersperse "\n    | " (reverse blocks))
        , "    deriving (Enum, Bounded, Eq, Ord, Ix, Show)"
        , ""
        , "-- | Block definition: range and name."
        , "--"
        , "-- @since 0.3.1"
        , "data BlockDefinition = BlockDefinition"
        , "    { blockRange :: !(Int, Int) -- ^ Range"
        , "    , blockName :: !String -- ^ Name"
        , "    } deriving (Eq, Ord, Show)"
        , ""
        , "-- | Block definition"
        , "--"
        , "-- @since 0.3.1"
        , "blockDefinition :: Block -> BlockDefinition"
        , "blockDefinition b = case b of"
        , mconcat (reverse defs)
        , "-- | Character block, if defined."
        , "--"
        , "-- @since 0.3.1"
        , "block :: Char -> Maybe Int"
        , "block (C# c#) = getBlock 0# " <> shows (length ranges - 1) "#"
        , "    where"
        , "    -- [NOTE] Encoding"
        , "    -- A range is encoded as two LE Word32:"
        , "    -- • First one is the lower bound, where the higher 11 bits are the block"
        , "    --   index and the lower 21 bits are the codepoint."
        , "    -- • Second one is the upper bound, which correspond to the codepoint."
        , ""
        , "    cp# = int2Word# (ord# c#)"
        , ""
        , "    -- Binary search"
        , "    getBlock l# u# = if isTrue# (l# ># u#)"
        , "        then Nothing"
        , "        else"
        , "            let k# = l# +# uncheckedIShiftRL# (u# -# l#) 1#"
        , "                j# = k# `uncheckedIShiftL#` 1#"
        , "                cpL0# = getRawCodePoint# j#"
        , "                cpL# = cpL0# `and#` 0x1fffff## -- Mask for codepoint: [0..0x10fff]"
        , "                cpU# = getRawCodePoint# (j# +# 1#)"
        , "            in if isTrue# (cpU# `ltWord#` cp#)"
        , "                -- cp > upper bound"
        , "                then getBlock (k# +# 1#) u#"
        , "                -- check lower bound"
        , "                else if isTrue# (cp# `ltWord#` cpL#)"
        , "                    -- cp < lower bound"
        , "                    then getBlock l# (k# -# 1#)"
        , "                    -- cp in block: get block index"
        , "                    else let block# = cpL0# `uncheckedShiftRL#` 21#"
        , "                         in Just (I# (word2Int# block#))"
        , ""
        , "    getRawCodePoint# k# ="
        , "#ifdef WORDS_BIGENDIAN"
        , "#if MIN_VERSION_base(4,16,0)"
        , "        byteSwap32# (word32ToWord# (indexWord32OffAddr# addr# k#))"
        , "#else"
        , "        byteSwap32# (indexWord32OffAddr# ranges# k#)"
        , "#endif"
        , "#elif MIN_VERSION_base(4,16,0)"
        , "        word32ToWord# (indexWord32OffAddr# ranges# k#)"
        , "#else"
        , "        indexWord32OffAddr# ranges# k#"
        , "#endif"
        , ""
        , "    -- Encoded ranges"
        , "    ranges# = \"" <> enumMapToAddrLiteral (mkRanges ranges') "\"#"
        ]

    initial :: ([String], [String], [(Int, Int)])
    initial = (mempty, mempty, mempty)

    step (blocks, defs, ranges) (blockName, blockRange) =
        let blockID = mkHaskellConstructor blockName
        in ( mkBlockConstructor blockID blockName blockRange : blocks
           , mkBlockDef   blockID blockName blockRange : defs
           , blockRange : ranges )

    mkBlockConstructor blockID blockName (l, u) = mconcat
        [ blockID
        , " -- ^ @U+"
        , showPaddedHeX l
        , "..U+"
        , showPaddedHeX u
        , "@: "
        , blockName
        , "."
        ]

    mkBlockDef blockID blockName (l, u) = mconcat
        [ "    "
        , blockID
        , " -> BlockDefinition (0x"
        , showPaddedHex l
        , ", 0x"
        , showPaddedHex u
        , ") "
        , show blockName
        , "\n"
        ]

    -- [NOTE] Encoding: a range is encoded as two LE Word32:
    -- • First one is the lower bound, where the higher 11 bits are the block
    --   index and the lower 21 bits are the codepoint.
    -- • Second one is upper bound, which correspond to the codepoint.
    mkRanges :: [(Int, Int)] -> [Word8]
    mkRanges = foldMap (uncurry mkBlockRange) . zip [0..]
    mkBlockRange :: Word32 -> (Int, Int) -> [Word8]
    mkBlockRange idx (l, u) = encodeBound idx l <> encodeBound 0 u

    encodeBound :: Word32 -> Int -> [Word8]
    encodeBound idx n = word32ToWord8s ((idx `shiftL` 21) .|. fromIntegral n)

defaultScript :: String
defaultScript = "Unknown"

genScriptsModule
    :: Monad m
    => String
    -> PropertyValuesAliases
    -> Fold m ScriptLine String
genScriptsModule moduleName aliases =
    done <$> Fold.foldl' addRange mempty
    where

    done ranges =
        let scripts = Set.toList
                        (foldr addScript (Set.singleton defaultScript) ranges)
        in unlines
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
            , "import GHC.Exts (Ptr(..))"
            , "import Unicode.Internal.Bits (lookupIntN)"
            , ""
            , "-- | Unicode [script](https://www.unicode.org/reports/tr24/)."
            , "--"
            , "-- The constructors descriptions are the original Unicode values"
            , "-- (short and long forms)."
            , "--"
            , "-- There is a total of " <> show (length scripts) <> " scripts."
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

    addRange :: [ScriptLine] -> ScriptLine -> [ScriptLine]
    addRange acc l@(script, r) = case acc of
        (script', r'):acc' -> if script == script'
            then case combineRanges r r' of
                Left  r'' -> (script, r'') : acc
                Right r'' -> (script, r'') : acc'
            else l : acc
        _ -> [l]

    combineRanges :: CharRange -> CharRange -> Either CharRange CharRange
    combineRanges r = case r of
        Left c1 -> \case
            Left c2 -> if c1 == succ c2
                then Right (Right (c2, c1))
                else Left r
            Right (c2, c3) -> if c1 == succ c3
                then Right (Right (c2, c1))
                else Left r
        Right (c1, c2) -> \case
            Left c3 -> if c1 == succ c3
                then Right (Right (c3, c2))
                else Left r
            Right (c3, c4) -> if c1 == succ c4
                then Right (Right (c3, c2))
                else Left r

    addScript :: ScriptLine -> Set.Set String -> Set.Set String
    addScript (script, _) = Set.insert script

    mkScripts :: [String] -> String
    mkScripts
        = mconcat
        . intersperse "\n  | "
        . fmap (\script -> mconcat
            [ mkHaskellConstructor script
            , " -- ^ "
            , case Map.lookup script aliasesMap of
                Just as -> mkAliases as
                Nothing -> error ("No abbreviation for script: " <> script)
            , ": @"
            , script
            , "@"
            ])

    -- Map: script long form → short forms
    aliasesMap :: Map.Map String [String]
    aliasesMap = Map.foldrWithKey
        (\abbr as -> Map.insert (head as) (abbr : tail as))
        mempty
        aliases
    mkAliases
        = mconcat
        . intersperse ", "
        . fmap (\abbr -> mconcat ["@", abbr, "@"])

    mkScriptDefinitions :: [ScriptLine] -> String
    mkScriptDefinitions
        = foldMap mkScriptDefinition
        . groupBy ((==) `on` fst)
        . reverse
        . addUnknownRanges

    addUnknownRanges :: [ScriptLine] -> [ScriptLine]
    addUnknownRanges ls =
        let addUnknown (acc, expected) (c, _) = case mkMissingRange expected c of
                Just r -> (,succ c) $ case acc of
                    r':acc' -> either (:acc) (:acc') (combineRanges r r')
                    _       -> [r]
                Nothing -> (acc, succ expected)
            addRest (acc@(r':acc'), expected) =
                let r = Right (expected, maxBound)
                in either (:acc) (:acc') (combineRanges r r')
            addRest _ = error "impossible"
            unknown = fmap (defaultScript,) . addRest $ foldl'
                addUnknown
                (mempty, '\0')
                (sort (foldMap (rangeToCharScripts id) ls))
        in unknown <> ls

    mkMissingRange :: Char -> Char -> Maybe CharRange
    mkMissingRange expected c
        | c == expected      = Nothing
        | c == succ expected = Just (Left expected)
        | otherwise          = Just (Right (expected, pred c))

    mkScriptDefinition :: [ScriptLine] -> String
    mkScriptDefinition ranges = mconcat
        [ "  "
        , mkHaskellConstructor (fst (head ranges))
        , " -> (Ptr \""
        , foldMap encodeRange ranges
        , "\"#, "
        , show (foldr (\r -> either (const (+1)) (const (+2)) (snd r)) 0 ranges :: Word)
        , ")\n"
        ]

    -- Encoding:
    -- • A single char is encoded as an LE Int32.
    -- • A range is encoded as two LE Int32 (first is lower bound, second is
    --   upper bound), which correspond to the codepoints with the 32th bit set.
    encodeRange :: ScriptLine -> String
    encodeRange (_, r) = case r of
        Left  c      -> encodeBytes (fromIntegral (ord c))
        Right (l, u) -> encodeBytes (setBit (fromIntegral (ord l)) 31)
                     <> encodeBytes (setBit (fromIntegral (ord u)) 31)
    encodeBytes = foldr addByte "" . word32ToWord8s
    addByte n acc = '\\' : shows n acc

    mkCharScripts :: [String] -> [ScriptLine] -> String
    mkCharScripts scripts scriptsRanges =
        let charScripts = sort (foldMap (rangeToCharScripts getScript) scriptsRanges)
            charScripts' = fst (foldl' addMissing (mempty, '\0') charScripts)
            addMissing (acc, expected) x@(c, script) = if expected < c
                then addMissing (def:acc, succ expected) x
                else (script:acc, succ c)
            def = getScript defaultScript
            getScript s = fromMaybe (error "script not found") (elemIndex s scripts)
        in genEnumBitmap "script" def (reverse charScripts')

    rangeToCharScripts :: (String -> b) -> ScriptLine -> [(Char, b)]
    rangeToCharScripts f (script, r) = case r of
        Left  cp     -> [(cp, f script)]
        Right (l, u) -> (, f script) <$> [l..u]

genScriptExtensionsModule
    :: Monad m
    => String
    -> PropertyValuesAliases
    -> ScriptExtensions
    -> Fold m ScriptLine String
genScriptExtensionsModule moduleName aliases extensions =
    done <$> Fold.foldl' processLine mempty

    where

    -- [NOTE] We rely on all the scripts having a short form

    -- Map: script → short form
    scriptsAbbr :: Map.Map String String
    scriptsAbbr =
        Map.foldrWithKey (\abbr as -> Map.insert (head as) abbr) mempty aliases
    getScriptAbbr :: String -> String
    getScriptAbbr = fromMaybe (error "script not found") . (scriptsAbbr Map.!?)

    -- All possible values: extensions + scripts
    extensionsSet :: Set.Set [String]
    extensionsSet = Set.fromList (Map.elems extensions)
                  <> Set.map pure (Map.keysSet aliases)
    extensionsList = sortBy
        (compare `on` fmap mkScript)
        (Set.toList extensionsSet)

    encodeExtensions :: [String] -> Int
    encodeExtensions e = fromMaybe
        (error ("extension not found: " <> show e))
        (elemIndex e extensionsList)

    encodedExtensions :: Map.Map [String] Int
    encodedExtensions =
        let l = length extensionsSet
        in if length extensionsSet > 0xff
            then error ("Too many script extensions: " <> show l)
            else Map.fromSet encodeExtensions extensionsSet

    processLine
        :: (Set.Set [String], Map.Map Char Int) -- used exts, encoded char exts
        -> ScriptLine
        -> (Set.Set [String], Map.Map Char Int)
    processLine acc (script, range) = case range of
        Left c         -> addChar script c acc
        Right (c1, c2) -> foldr (addChar script) acc [c1..c2]

    addChar
        :: String -- script
        -> Char   -- processed char
        -> (Set.Set [String], Map.Map Char Int)
        -> (Set.Set [String], Map.Map Char Int)
    addChar script c (extsAcc, charAcc) = case Map.lookup c extensions of
        -- Char has explicit extensions
        Just exts -> ( Set.insert exts extsAcc
                     , Map.insert c (encodedExtensions Map.! exts) charAcc)
        -- Char has no explicit extensions: use its script
        Nothing   ->
            let exts = [getScriptAbbr script]
            in ( Set.insert exts extsAcc
               , Map.insert c (encodedExtensions Map.! exts) charAcc)

    done (usedExts, exts) = unlines
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
        , "import Unicode.Internal.Char.Scripts (Script(..))"
        , "import Unicode.Internal.Bits (lookupIntN)"
        , ""
        , "-- | Useful to decode the output of 'scriptExtensions'."
        , "decodeScriptExtensions :: Int -> NonEmpty Script"
        , "decodeScriptExtensions = \\case" <> mkDecodeScriptExtensions usedExts
        , "    _   -> [" <> mkHaskellConstructor defaultScript <> "]"
        , ""
        , "-- | Script extensions of a character."
        , "--"
        , "-- @since 0.1.0"
        , genEnumBitmap "scriptExtensions" def (mkScriptExtensions exts)
        ]

    mkDecodeScriptExtensions :: Set.Set [String] -> String
    mkDecodeScriptExtensions
        = mkDecodeScriptExtensions'
        . Set.map (\exts -> (encodedExtensions Map.! exts, exts))
    mkDecodeScriptExtensions' = foldMap $ \(v, exts) -> mconcat
        [ "\n    "
        , show v
        , " -> ["
        , mconcat (intersperse ", " (mkScript <$> exts))
        , "]"
        ]
    mkScript :: String -> String
    mkScript = mkHaskellConstructor . head . (aliases Map.!)

    def :: Int
    def = encodedExtensions Map.! [getScriptAbbr defaultScript]

    mkScriptExtensions
        = reverse
        . snd
        . Map.foldlWithKey addCharExt ('\0', mempty)
    addCharExt (expected, acc) c v = if expected < c
        then addCharExt (succ expected, def : acc) c v
        else (succ c, v : acc)

-------------------------------------------------------------------------------
-- Parsing UnicodeData.txt
-------------------------------------------------------------------------------

genGeneralCategoryModule
    :: Monad m
    => String
    -> Fold m DetailedChar String
genGeneralCategoryModule moduleName =
    done <$> Fold.foldl' step initial

    where

    -- (categories, expected char)
    initial = ([], '\0')

    step (acc, p) a = if p < _char a
        -- Fill missing char entry with default category Cn
        -- See: https://www.unicode.org/reports/tr44/#Default_Values_Table
        then step (Cn : acc, succ p) a
        -- Regular entry
        else (_generalCategory a : acc, succ (_char a))

    done (acc, _) = unlines
        [ apacheLicense 2020 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(generalCategory)"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Unicode.Internal.Bits (lookupIntN)"
        , ""
        , genEnumBitmap "generalCategory" Cn (reverse acc)
        ]

readDecomp :: String -> (Maybe DecompType, Decomp)
readDecomp s =
    if null wrds
    then (Nothing, DCSelf)
    else decmps wrds

    where

    decmps [] = error "Unreachable flow point"
    decmps y@(x:xs) =
        case dtmap x of
            DTCanonical -> (,) (Just DTCanonical) (readCP y)
            other -> (,) (Just other) (readCP xs)

    wrds = words s

    readCP ws = DC $ map readCodePoint ws

    dtmap "<compat>" = DTCompat
    dtmap "<circle>" = DTCircle
    dtmap "<final>" = DTFinal
    dtmap "<font>" = DTFont
    dtmap "<fraction>" = DTFraction
    dtmap "<initial>" = DTInitial
    dtmap "<isolated>" = DTIsolated
    dtmap "<medial>" = DTMedial
    dtmap "<narrow>" = DTNarrow
    dtmap "<noBreak>" = DTNoBreak
    dtmap "<small>" = DTSmall
    dtmap "<square>" = DTSquare
    dtmap "<sub>" = DTSub
    dtmap "<super>" = DTSuper
    dtmap "<vertical>" = DTVertical
    dtmap "<wide>" = DTWide
    dtmap _ = DTCanonical

filterNonHangul :: Monad m => Fold m DetailedChar a -> Fold m DetailedChar a
filterNonHangul = Fold.filter (not . isHangul . _char)

filterDecomposableType ::
       Monad m => DType -> Fold m DetailedChar a -> Fold m DetailedChar a
filterDecomposableType dtype =
    Fold.filter ((/= DCSelf) . _decomposition)
        . Fold.filter (predicate . _decompositionType)

    where

    predicate =
        case dtype of
            Canonical -> (== Just DTCanonical)
            Kompat -> const True

genDecomposableModule ::
       Monad m => String -> DType -> Fold m DetailedChar String
genDecomposableModule moduleName dtype =
    filterNonHangul
        $ filterDecomposableType dtype $ done <$> Fold.foldl' step initial

    where

    initial = []

    step st a = ord (_char a) : st

    done st =
        unlines
            [ apacheLicense 2020 moduleName
            , "{-# OPTIONS_HADDOCK hide #-}"
            , ""
            , "module " <> moduleName
            , "(isDecomposable)"
            , "where"
            , ""
            , "import Data.Char (ord)"
            , "import Unicode.Internal.Bits (lookupBit64)"
            , ""
            , genBitmap "isDecomposable" (reverse st)
            ]

genCombiningClassModule :: Monad m => String -> Fold m DetailedChar String
genCombiningClassModule moduleName =
    Fold.filter (\dc -> _combiningClass dc /= 0)
        $ done <$> Fold.foldl' step initial

    where

    initial = ([], [])

    step (st1, st2) a = (genCombiningClassDef a : st1, ord (_char a) : st2)

    done (st1, st2) =
        unlines
            [ apacheLicense 2020 moduleName
            , "{-# LANGUAGE LambdaCase #-}"
            , "{-# OPTIONS_HADDOCK hide #-}"
            , "module " <> moduleName
            , "(combiningClass, isCombining)"
            , "where"
            , ""
            , "import Data.Char (ord)"
            , "import Unicode.Internal.Bits (lookupBit64)"
            , ""
            , "combiningClass :: Char -> Int"
            , "combiningClass = \\case"
            , unlines (reverse st1)
            , "  _ -> 0\n"
            , ""
            , genBitmap "isCombining" (reverse st2)
            ]

    genCombiningClassDef dc = mconcat
        [ "  "
        , show (_char dc)
        , " -> "
        , show (_combiningClass dc)
        ]

genDecomposeDefModule ::
       Monad m
    => String
    -> [String]
    -> [String]
    -> DType
    -> (Int -> Bool)
    -> Fold m DetailedChar String
genDecomposeDefModule moduleName before after dtype predicate =
    Fold.filter (predicate . ord . _char)
        $ filterNonHangul
        $ filterDecomposableType dtype $ done <$> Fold.foldl' step initial

    where

    decomposeChar c DCSelf = [c]
    decomposeChar _c (DC ds) = ds

    genHeader =
        [ apacheLicense 2020 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(decompose)"
        , "where"
        , ""
        ]
    genSign =
        [ "{-# NOINLINE decompose #-}"
        , "decompose :: Char -> [Char]"
        , "decompose = \\case"
        ]
    initial = []

    step st dc = genDecomposeDef dc : st

    done st =
        let body = mconcat [genHeader, before, genSign, reverse st, after]
        in unlines body

    genDecomposeDef dc = mconcat
        [ "  "
        , show (_char dc)
        , " -> "
        , show (decomposeChar (_char dc) (_decomposition dc))
        ]

genCompositionsModule ::
       Monad m
    => String
    -> [Int]
    -> [Int]
    -> Fold m DetailedChar String
genCompositionsModule moduleName compExclu non0CC =
    Fold.filter (not . flip elem compExclu . ord . _char)
        $ filterNonHangul
        $ Fold.filter (isDecompositionLen2 . _decomposition)
        $ filterDecomposableType Canonical $ done <$> Fold.foldl' step initial

    where

    isDecompositionLen2 DCSelf = False
    isDecompositionLen2 (DC ds) = length ds == 2

    genComposePairDef name dc =
        name
            <> " "
            <> show (head d01)
            <> " " <> show (d01 !! 1) <> " = Just " <> show (_char dc)

        where

        d01 = decompPair dc

    decompPair dc =
        case _decomposition dc of
            DCSelf -> error "toCompFormat: DCSelf"
            (DC ds) ->
                if length ds == 2
                then ds
                else error "toCompFormat: length /= 2"

    initial = ([], [], [])

    step (dec, sp, ss) dc = (dec1, sp1, ss1)

        where

        d01 = decompPair dc
        d1Ord = ord $ d01 !! 1
        dec1 = genComposePairDef "compose" dc : dec
        sp1 =
            if d1Ord `notElem` non0CC
            then genComposePairDef "composeStarters" dc : sp
            else sp
        ss1 =
            if d1Ord `notElem` non0CC
            then d1Ord : ss
            else ss

    header =
        [ apacheLicense 2020 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(compose, composeStarters, isSecondStarter)"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Unicode.Internal.Bits (lookupBit64)"
        , ""
        ]

    composePair decomps =
        [ "{-# NOINLINE compose #-}"
        , "compose :: Char -> Char -> Maybe Char"
        , unlines decomps
        , "compose _ _ = " <> "Nothing" <> "\n"
        , ""
        ]

    composeStarterPair starterPairs =
        [ "composeStarters :: Char -> Char -> Maybe Char"
        , unlines starterPairs
        , "composeStarters _ _ = " <> "Nothing" <> "\n"
        , ""
        ]

    isSecondStarter secondStarters =
        [genBitmap "isSecondStarter" secondStarters]

    done (dec, sp, ss) =
        unlines
            $ header
            ++ composePair (reverse dec)
            ++ composeStarterPair (reverse sp)
            ++ isSecondStarter (Set.toList (Set.fromList ss))

genSimpleCaseMappingModule
    :: Monad m
    => String
    -> String
    -> (DetailedChar -> Maybe Char)
    -> Fold m DetailedChar String
genSimpleCaseMappingModule moduleName funcName field =
    done <$> Fold.foldl' step initial

    where

    genHeader =
        [ apacheLicense 2020 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(" <> funcName <> ")"
        , "where"
        , ""
        ]
    genSign =
        [ "{-# NOINLINE " <> funcName <> " #-}"
        , funcName <> " :: Char -> Char"
        , funcName <> " = \\case"
        ]
    initial = []

    step ds dc = case mkEntry dc of
        Nothing -> ds
        Just d  -> d : ds

    after = ["  c -> c"]

    done st =
        let body = mconcat [genHeader, genSign, reverse st, after]
        in unlines body

    mkEntry dc = field dc <&> \c -> mconcat
        [ "  "
        , show (_char dc)
        , " -> "
        , show c
        ]

-- [NOTE] Case mapping encodes up to 3 code points on 21 bits each in an Int64.
genSpecialCaseMappingModule
    :: Monad m
    => String
    -> String
    -> SpecialCasings
    -- ^ Special casings
    -> (SpecialCasing -> String)
    -- ^ Special case selector
    -> (DetailedChar -> Maybe Char)
    -- ^ Simple case selector
    -> Fold m DetailedChar String
genSpecialCaseMappingModule moduleName funcName specialCasings special simple =
    done <$> Fold.foldl' step initial

    where

    genHeader =
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
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

    step xs dc = case mkEntry dc of
        Nothing -> xs
        Just x  -> x : xs

    after = ["  _ -> 0"]

    done st =
        let body = mconcat [genHeader, reverse st, after]
        in unlines body

    mkEntry dc = (mkSpecial dc <|> mkSimple dc) <&> \k -> mconcat
        [ "  "
        , show (_char dc)
        , " -> 0x"
        , showHex k ""
        ]

    mkSimple = fmap ord . simple
    mkSpecial = fmap (encode . special) . (specialCasings Map.!?) . _char
    encode :: String -> Int
    encode
        = foldr (\(k, c) -> (+) (ord c `shiftL` k)) 0
        . zip [0, 21, 42]
        -- Check min 1 character, max 3 characters
        . (\cs -> if null cs || length cs > 3 then error (show cs) else cs)

-- [NOTE] Case folding encodes up to 3 code points on 21 bits each in an Int64.
genCaseFolding
    :: Monad m
    => String
    -> Fold m CaseFoldings String
genCaseFolding moduleName =
    done <$> Fold.foldl' step initial

    where

    genHeader =
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
        , "toCasefold = \\case"
        ]
    initial = []

    step xs cf = maybe xs (:xs) (mkEntry cf)

    after = ["  _ -> 0"]

    done st =
        let body = mconcat [genHeader, reverse st, after]
        in unlines body

    mkEntry (c, cfs)
        = (lookup FullCaseFolding cfs <|> lookup CommonCaseFolding cfs)
        <&> \cf -> mconcat
            [ "  "
            , show c
            , " -> 0x"
            , showHex (encode cf) ""
            ]

    encode :: String -> Int
    encode
        = foldr (\(k, c) -> (+) (ord c `shiftL` k)) 0
        . zip [0, 21, 42]
        -- Check min 1 character, max 3 characters
        . (\cs -> if null cs || length cs > 3 then error (show cs) else cs)

genCorePropertiesModule ::
       Monad m => String -> (String -> Bool) -> Fold m (String, [Int]) String
genCorePropertiesModule moduleName isProp =
    Fold.filter (\(name, _) -> isProp name) $ done <$> Fold.foldl' step initial

    where

    prop2FuncName x = "is" ++ x

    initial = ([], [])

    step (props, bitmaps) (name, bits) =
        (name : props, genBitmap (prop2FuncName name) bits : bitmaps)

    done (props, bitmaps) = unlines $ header props ++ bitmaps

    header exports =
        [ apacheLicense 2020 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(" ++ unwords (intersperse "," (map prop2FuncName exports)) ++ ")"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Unicode.Internal.Bits (lookupBit64)"
        ]

genNamesModule
    :: Monad m
    => String
    -> Fold m CharName String
genNamesModule moduleName =
    done <$> Fold.foldl' addCharName (mempty, mempty, 0)

    where

    addCharName (names, offsets, offset) (CharName char name) =
        ( name <> ['\0'] : names
        , encodeOffset char offset : offsets
        , offset + length name + 1
        )

    encodeOffset char offset = encode32LE (ord char) (encode32LE offset mempty)
    encode32LE v acc
        = (v             .&. 0xff)
        : (v `shiftR` 8  .&. 0xff)
        : (v `shiftR` 16 .&. 0xff)
        :  v `shiftR` 24
        : acc

    done (names, offsets, _) = unlines
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE CPP #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(name)"
        , "where"
        , ""
        , "#include \"MachDeps.h\""
        , ""
        , "import Foreign.C.String (CString)"
        , "import GHC.Exts"
        , ""
        , "-- | Name of a character, if defined."
        , "--"
        , "-- @since 0.1.0"
        , "{-# INLINE name #-}"
        , "name :: Char -> Maybe CString"
        , "name (C# c#) = getName 0# " <> shows (length names - 1) "#"
        , "    where"
        , "    -- [NOTE] Encoding"
        , "    -- • The names are ASCII. Each name is encoded as a NUL-terminated CString."
        , "    -- • The names are concatenated in names#."
        , "    -- • The name of a character, if defined, is referenced by an offset in names#."
        , "    -- • The offsets are stored in offsets#. A character entry is composed of two"
        , "    --   LE Word32: the first one is the codepoint, the second one is the offset."
        , "    -- • We use binary search on offsets# to extract names from names#."
        , "    cp# = ord# c#"
        , "    -- Binary search"
        , "    getName l# u# = if isTrue# (l# ># u#)"
        , "        then Nothing"
        , "        else"
        , "            let k# = l# +# uncheckedIShiftRL# (u# -# l#) 1#"
        , "                j# = k# `uncheckedIShiftL#` 1#"
        , "                cp'# = indexInt32OffAddr'# j#"
        , "            in if isTrue# (cp'# <# cp#)"
        , "                then getName (k# +# 1#) u#"
        , "                else if isTrue# (cp'# ==# cp#)"
        , "                    then let offset# = indexInt32OffAddr'# (j# +# 1#)"
        , "                         in Just (Ptr (names# `plusAddr#` offset#))"
        , "                    else getName l# (k# -# 1#)"
        , "    indexInt32OffAddr'# :: Int# -> Int#"
        , "    indexInt32OffAddr'# k# ="
        , "#ifdef WORDS_BIGENDIAN"
        , "#if MIN_VERSION_base(4,16,0)"
        , "        word2Int# (byteSwap32# (word32ToWord# (indexWord32OffAddr# offsets# k#)))"
        , "#else"
        , "        word2Int# (byteSwap32# (indexWord32OffAddr# offsets# k#))"
        , "#endif"
        , "#elif MIN_VERSION_base(4,16,0)"
        , "        int32ToInt# (indexInt32OffAddr# offsets# k#)"
        , "#else"
        , "        indexInt32OffAddr# offsets# k#"
        , "#endif"
        , ""
        , "    names# = "
        -- Note: names are ASCII
            <> shows (mconcat (reverse names)) "#"
        , "    offsets# = \""
            <> enumMapToAddrLiteral (mconcat (reverse offsets)) "\"#"
        ]

genAliasesModule
    :: Monad m
    => String
    -> Fold m CharAliases String
genAliasesModule moduleName =
    done <$> Fold.foldr (\a -> (:) (mkCharAliases a)) mempty

    where

    mkCharAliases :: CharAliases -> String
    mkCharAliases (CharAliases char aliases) = mconcat
        [ "  '\\x"
        , showHexCodepoint char
        , "' -> "
        , show . Map.toList
               . Map.fromListWith (flip (<>))
               $ fmap ((:[])) <$> aliases
        , "\n"
        ]

    done names = unlines
        [ apacheLicense 2022 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(NameAliasType(..), nameAliases, nameAliasesByType, nameAliasesWithTypes)"
        , "where"
        , ""
        , "import Data.Ix (Ix)"
        , "import Data.Maybe (fromMaybe)"
        , "import Foreign.C.String (CString)"
        , "import GHC.Exts (Ptr(..))"
        , ""
        , "-- | Type of name alias. See Unicode Standard 14.0.0, section 4.8."
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
        , "    deriving (Enum, Bounded, Eq, Ord, Ix, Show)"
        , ""
        , "-- | All name aliases of a character."
        , "-- The names are listed in the original order of the UCD."
        , "--"
        , "-- See 'nameAliasesWithTypes' for the detailed list by alias type."
        , "--"
        , "-- @since 0.1.0"
        , "{-# INLINE nameAliases #-}"
        , "nameAliases :: Char -> [CString]"
        , "nameAliases = mconcat . fmap snd . nameAliasesWithTypes"
        , ""
        , "-- | Name aliases of a character for a specific name alias type."
        , "--"
        , "-- @since 0.1.0"
        , "{-# INLINE nameAliasesByType #-}"
        , "nameAliasesByType :: NameAliasType -> Char -> [CString]"
        , "nameAliasesByType t = fromMaybe mempty . lookup t . nameAliasesWithTypes"
        , ""
        , "-- | Detailed character names aliases."
        , "-- The names are listed in the original order of the UCD."
        , "--"
        , "-- See 'nameAliases' if the alias type is not required."
        , "--"
        , "-- @since 0.1.0"
        , "nameAliasesWithTypes :: Char -> [(NameAliasType, [CString])]"
        , "nameAliasesWithTypes = \\case"
        , mconcat names
        , "  _ -> mempty"
        ]

genNumericValuesModule
    :: Monad m
    => String
    -> Fold m CharNumericValue String
genNumericValuesModule moduleName =
    done . foldMap mkNumericValue . sort <$> Fold.toListRev

    where

    mkNumericValue (char, value) = mconcat
        [ "\n  "
        , show char
        , " -> "
        , either show show (bimap Just Just value)
        ]

    done values = unlines
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(numericValue)"
        , "where"
        , ""
        , "import Data.Ratio ((%))"
        , ""
        , "numericValue :: Char -> Maybe Rational"
        , "numericValue = \\case" <> values
        , "  _ -> Nothing"
        ]

genIdentifierStatusModule
    :: Monad m
    => String
    -> Fold m CharIdentifierStatus String
genIdentifierStatusModule moduleName =
    done <$> Fold.lmap mkAllowed Fold.toList

    where

    mkAllowed = \case
        CharIdentifierStatus c Allowed -> ord c
        x                              -> error ("Unexpected " <> show x)

    done values = unlines
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(isAllowedInIdentifier)"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Unicode.Internal.Bits (lookupBit64)"
        , ""
        , genBitmap "isAllowedInIdentifier" values
        ]

genIdentifierTypeModule
    :: Monad m
    => String
    -> Fold m CharIdentifierTypes String
genIdentifierTypeModule moduleName =
    done . mkIdentifiersTypes <$> Fold.foldl' addIdentifierType mempty

    where

    addIdentifierType
        :: Map.Map Char [IdentifierType]
        -> CharIdentifierTypes
        -> Map.Map Char [IdentifierType]
    addIdentifierType acc (CharIdentifierTypes c types) =
        Map.insertWith (flip (<>)) c types acc

    mkIdentifiersTypes
        :: Map.Map Char [IdentifierType]
        -> (String, [Int])
    mkIdentifiersTypes types =
        let encoding = Set.toList (Set.fromList (def : Map.elems types))
        in assert (length encoding < 0xff)
            ( foldMap addEncoding (zip [0..] encoding)
            , snd (Map.foldlWithKey' (addChar encoding) ('\0', mempty) types) )

    -- Default value
    def = [Not_Character]

    addEncoding :: (Int, [IdentifierType]) -> String
    addEncoding (n, e) = mconcat
        [ "\n    "
        , show n
        , " -> "
        , hackHaskellConstructor e ]

    addChar
        :: [[IdentifierType]]
        -> (Char, [Int])
        -> Char
        -> [IdentifierType]
        -> (Char, [Int])
    addChar encoding (expected, acc) c types = if expected < c
        then
            let acc' = encodeTypes encoding def : acc
            in addChar encoding (succ expected, acc') c types
        else (succ c, encodeTypes encoding types : acc)

    encodeTypes :: [[IdentifierType]] -> [IdentifierType] -> Int
    encodeTypes encoding types
        = assert (elemIndex def encoding == Just 0)
        $ fromMaybe 0 (elemIndex types encoding)

    hackHaskellConstructor = filter (/= '_') . show

    done (encoding, identifiersTypes) = unlines
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE LambdaCase, OverloadedLists #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(IdentifierType(..), identifierTypes, decodeIdentifierTypes)"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Data.List.NonEmpty (NonEmpty)"
        , "import Unicode.Internal.Bits (lookupIntN)"
        , ""
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
        , "    _ -> " <> hackHaskellConstructor def
        , ""
        , "-- | Returns the 'IdentifierType's corresponding to a character."
        , genEnumBitmap "identifierTypes" 0 (reverse identifiersTypes)
        ]

genConfusablesModule
    :: Monad m
    => String
    -> Fold m Confusable String
genConfusablesModule moduleName =
    done . foldMap mkConfusable . sort <$> Fold.toList

    where

    mkConfusable :: Confusable -> String
    mkConfusable (Confusable c s) = mconcat
        [ "\n    "
        , show c
        , " -> Just (Ptr \""
        , stringToAddrLiteral s
        , "\\0\"#)"
        ]

    -- Encode string as a null-terminated utf-8
    stringToAddrLiteral = foldMap toWord8 . encodeUtf8
    -- [HACK] Encode in utf-8, then decode in latin1 in order to get bytes
    encodeUtf8 s
        = unsafePerformIO
        $ Foreign.withCString Encoding.utf8 s
            (Foreign.peekCString Encoding.latin1)
    toWord8 = ('\\' :) . show . ord

    done confusables = unlines
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(confusablePrototype)"
        , "where"
        , ""
        , "import Foreign.C.String (CString)"
        , "import GHC.Exts (Ptr(..))"
        , ""
        , "-- | Returns the /prototype/ of a character, if it is confusable."
        , "--"
        , "-- The resulting 'CString' is null-terminated and encoded in UTF-8."
        , "--"
        , "-- @since 0.1.0"
        , "confusablePrototype :: Char -> Maybe CString"
        , "confusablePrototype = \\case" <> confusables
        , "    _ -> Nothing"
        ]

genIntentionalConfusablesModule
    :: Monad m
    => String
    -> Fold m IntentionalConfusable String
genIntentionalConfusablesModule moduleName =
    done . Map.foldMapWithKey mkConfusable <$> Fold.foldl' addEntry mempty

    where

    addEntry
        :: Map.Map Char (Set.Set Char)
        -> IntentionalConfusable
        -> Map.Map Char (Set.Set Char)
    addEntry acc (IntentionalConfusable c1 c2)
        = Map.insertWith (flip (<>)) c1 (Set.singleton c2)
        . Map.insertWith (flip (<>)) c2 (Set.singleton c1)
        $ acc

    mkConfusable :: Char -> Set.Set Char -> String
    mkConfusable c cs = mconcat
        [ "\n    "
        , show c
        , " -> Just (Ptr \""
        , stringToAddrLiteral (Set.toList cs)
        , "\\0\"#)"
        ]

    -- Encode string as a null-terminated utf-8
    stringToAddrLiteral = foldMap toWord8 . encodeUtf8
    -- [HACK] Encode in utf-8, then decode in latin1 in order to get bytes
    encodeUtf8 s
        = unsafePerformIO
        $ Foreign.withCString Encoding.utf8 s
            (Foreign.peekCString Encoding.latin1)
    toWord8 = ('\\' :) . show . ord

    done confusables = unlines
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(intentionalConfusables)"
        , "where"
        , ""
        , "import Foreign.C.String (CString)"
        , "import GHC.Exts (Ptr(..))"
        , ""
        , "-- | Returns the /intentional/ confusables of a character, if any."
        , "--"
        , "-- The resulting 'CString' is null-terminated and encoded in UTF-8."
        , "--"
        , "-- @since 0.1.0"
        , "intentionalConfusables :: Char -> Maybe CString"
        , "intentionalConfusables = \\case" <> confusables
        , "    _ -> Nothing"
        ]

-------------------------------------------------------------------------------
-- Parsing blocks file
-------------------------------------------------------------------------------

type BlockLine = (String, (Int, Int))

parseBlockLine :: String -> Maybe BlockLine
parseBlockLine ln
    | null ln = Nothing
    | head ln == '#' = Nothing
    | otherwise = Just (parseLine ln)

    where

    parseLine line =
        let (rangeLn, line1) = span (/= ';') line
            name = takeWhile (/= '#') (tail line1)

        in (trim' name, parseRange (trim rangeLn))

    parseRange
        = bimap parseCodePoint (parseCodePoint . drop 2)
        . span (/= '.')

    parseCodePoint = read . ("0x" <>)

parseBlockLines
    :: (IsStream t, Monad m)
    => t m String
    -> t m BlockLine
parseBlockLines = Stream.mapMaybe parseBlockLine

-------------------------------------------------------------------------------
-- Parsing script file
-------------------------------------------------------------------------------

type ScriptLine = (String, Either Char (Char, Char))

parseScriptLine :: String -> Maybe ScriptLine
parseScriptLine ln
    | null ln = Nothing
    | head ln == '#' = Nothing
    | otherwise = Just (parseLine ln)

    where

    parseLine line =
        let (rangeLn, line1) = span (/= ';') line
            script = takeWhile (/= '#') (tail line1)

        in (trim script, parseRange (trim rangeLn))

    parseRange :: String -> Either Char (Char, Char)
    parseRange
        = (\(c1, c2) -> maybe (Left c1) (Right . (c1,)) c2)
        . bimap readCodePoint (readCodePointM . drop 2)
        . span (/= '.')

parseScriptLines
    :: (IsStream t, Monad m)
    => t m String
    -> t m ScriptLine
parseScriptLines = Stream.mapMaybe parseScriptLine

-------------------------------------------------------------------------------
-- Parsing ScriptExtensions.txt
-------------------------------------------------------------------------------

type ScriptExtensionsLine = (CharRangeStream, [String])

data CharScriptExtensions = CharScriptExtensions
    { _scriptExtensionsChar    :: !Char
    , _scriptExtensionsScripts :: ![String] }
type ScriptExtensions = Map.Map Char [String]

parseScriptExtensionsLine :: String -> Maybe ScriptExtensionsLine
parseScriptExtensionsLine = \case
    ""    -> Nothing -- empty line
    '#':_ -> Nothing -- comment
    line  -> Just (parseLine line)

    where

    parseLine line =
        let (rangeLn, line1) = span (/= ';') line
            range = parseCodePointRange rangeLn
            scripts = words (takeWhile (/= '#') (tail line1))
        in (either SingleChar (uncurry CharRange) range, scripts)

parseScriptExtensionsLines
    :: (IsStream t, Monad m)
    => t m String
    -> t m CharScriptExtensions
parseScriptExtensionsLines
    = Stream.unfoldMany (Unfold.unfoldr mkScriptExtension)
    . Stream.mapMaybe parseScriptExtensionsLine

    where

    mkScriptExtension
        :: ScriptExtensionsLine
        -> Maybe (CharScriptExtensions, ScriptExtensionsLine)
    mkScriptExtension (step, scripts) = case step of
        SingleChar c    -> Just (CharScriptExtensions c scripts, (Stop, mempty))
        CharRange c1 c2 -> Just ( CharScriptExtensions c1 scripts
                                , if c1 < c2
                                    then (CharRange (succ c1) c2, scripts)
                                    else (Stop, mempty) )
        Stop            -> Nothing

-------------------------------------------------------------------------------
-- Parsing property files
-------------------------------------------------------------------------------

trim :: String -> String
trim = takeWhile (not . isSpace) . dropWhile isSpace

trim' :: String -> String
trim' = dropWhileEnd isSpace . dropWhile isSpace

type PropertyLine = (String, [Int])

emptyPropertyLine :: PropertyLine
emptyPropertyLine = ("", [])

combinePropertyLines :: PropertyLine -> PropertyLine -> PropertyLine
combinePropertyLines t1@(n1, o1) t2@(n2, o2)
    | n1 == "" = t2
    | n2 == "" = t1
    | n1 == n2 = (n1, o1 ++ o2)
    | otherwise = error $ "Cannot group " ++ n1 ++ " with " ++ n2

parsePropertyLine :: String -> PropertyLine
parsePropertyLine ln
    | null ln = emptyPropertyLine
    | head ln == '#' = emptyPropertyLine
    | otherwise = parseLineJ ln

    where

    parseLineJ :: String -> (String, [Int])
    parseLineJ line =
        let (rangeLn, line1) = span (/= ';') line
            propLn = takeWhile (/= '#') (tail line1)
        in (trim propLn, parseRange (trim rangeLn))

    parseRange :: String -> [Int]
    parseRange rng =
        if '.' `elem` rng
        then let low = read $ "0x" ++ takeWhile (/= '.') rng
                 high =
                     read $ "0x" ++ reverse (takeWhile (/= '.') (reverse rng))
              in [low .. high]
        else [read $ "0x" ++ rng]

isDivider :: String -> Bool
isDivider x = x == "# ================================================"

parsePropertyLines :: (IsStream t, Monad m) => t m String -> t m PropertyLine
parsePropertyLines =
    Stream.splitOn isDivider
        $ Fold.lmap parsePropertyLine
        $ Fold.foldl' combinePropertyLines emptyPropertyLine

-------------------------------------------------------------------------------
-- Parsing UnicodeData.txt
-------------------------------------------------------------------------------

data PropertyValueAliasesLine = PropertyValueAliasesLine
    { _prop             :: !String
    , _propValue        :: !String
    , _propValueAliases :: [String] }

type PropertyValuesAliases      = Map.Map String [String]
type PropertyValuesAliasesEntry = (String, PropertyValuesAliases)

parsePropertyValueAliasesLine :: String -> Maybe PropertyValueAliasesLine
parsePropertyValueAliasesLine = \case
    ""    -> Nothing -- empty line
    '#':_ -> Nothing -- comment
    line  -> case split line of
        prop : value : as -> Just (PropertyValueAliasesLine prop value as)
        _                 -> error ("Unsupported line: " <> line)

    where

    split s = case s' of
        ""    -> [v']
        '#':_ -> [v']
        s''   -> v' : split (tail s'')
        where (v, s') = span (\c -> c /= ';' && c /= '#') (dropWhile isSpace s)
              v' = dropWhileEnd isSpace v

parsePropertyValueAliasesLines
    :: forall t m. (IsStream t, Monad m)
    => t m String
    -> t m PropertyValuesAliasesEntry
parsePropertyValueAliasesLines
    = Stream.groupsBy ((==) `on` _prop) (Fold.foldr addEntry mempty)
    . Stream.mapMaybe parsePropertyValueAliasesLine

    where

    addEntry (PropertyValueAliasesLine prop value aliases) (_, acc) =
        ( prop
        , Map.insert value aliases acc )

-------------------------------------------------------------------------------
-- Parsing UnicodeData.txt
-------------------------------------------------------------------------------

-- | A range entry in @UnicodeData.txt@.
data UnicodeDataRange
    = SingleCode    !DetailedChar
    -- ^ Regular entry for one code point
    | FirstCode     !String !DetailedChar
    -- ^ A partial range for entry with a name as: @\<RANGE_IDENTIFIER, First\>@
    | CompleteRange !String !DetailedChar !DetailedChar
    -- ^ A complete range, requiring 2 continuous entries with respective names:
    --
    -- * @\<RANGE_IDENTIFIER, First\>@
    -- * @\<RANGE_IDENTIFIER, Last\>@

{-| Parse UnicodeData.txt lines

Parse ranges according to https://www.unicode.org/reports/tr44/#Code_Point_Ranges.

__Note:__ this does /not/ fill missing char entries,
i.e. entries with no explicit entry nor within a range.
-}
parseUnicodeDataLines :: forall t m. (IsStream t, Monad m) => t m String -> t m DetailedChar
parseUnicodeDataLines
    = Stream.unfoldMany (Unfold.unfoldr unitToRange)
    . Stream.foldMany ( Fold.lmap parseDetailedChar
                      $ Fold.mkFold_ step initial )

    where

    step :: Maybe UnicodeDataRange
         -> DetailedChar
         -> Fold.Step (Maybe UnicodeDataRange) (Maybe UnicodeDataRange)
    step Nothing dc = case span (/= ',') (_name dc) of
        (range, ", First>") -> Fold.Partial (Just (FirstCode range dc))
        _                   -> Fold.Done (Just (SingleCode dc))
    step (Just (FirstCode range1 dc1)) dc2 = case span (/= ',') (_name dc2) of
        (range2, ", Last>") -> if range1 == range2 && _char dc1 < _char dc2
            then Fold.Done (Just (CompleteRange range1 dc1 dc2))
            else error $ "Cannot create range: incompatible ranges" <> show (dc1, dc2)
        _ -> error $ "Cannot create range: missing <range, Last> entry corresponding to: " <> show range1
    step _ _ = error "impossible case"

    initial :: Fold.Step (Maybe UnicodeDataRange) (Maybe UnicodeDataRange)
    initial = Fold.Partial Nothing

    unitToRange :: Maybe UnicodeDataRange -> Maybe (DetailedChar, Maybe UnicodeDataRange)
    unitToRange = fmap $ \case
        SingleCode          dc      -> (dc, Nothing)
        FirstCode     _     dc      -> error $ "Incomplete range: " <> show dc
        CompleteRange range dc1 dc2 -> if _char dc1 < _char dc2
            -- [TODO] Create the proper name
            then (dc1{_name="TODO"}, Just (CompleteRange range dc1{_char=succ (_char dc1)} dc2))
            else (dc2{_name="TODO"}, Nothing)

-- | Parse a single entry of @UnicodeData.txt@
parseDetailedChar :: String -> DetailedChar
parseDetailedChar line =
    DetailedChar
        { _char = readCodePoint char
        , _name = name
        , _generalCategory = read gc
        , _combiningClass = read combining
        , _decompositionType = dctype
        , _decomposition = dcval
        , _simpleUpperCaseMapping = readCodePointM sUpper
        , _simpleLowerCaseMapping = readCodePointM sLower
        , _simpleTitleCaseMapping = readCodePointM sTitle
        }

    where

    (char, line1) = span (/= ';') line
    (name, line2) = span (/= ';') (tail line1)
    (gc, line3) = span (/= ';') (tail line2)
    (combining, line4) = span (/= ';') (tail line3)
    (_bidi, line5) = span (/= ';') (tail line4)
    (decomposition, line6) = span (/= ';') (tail line5)
    (dctype, dcval) = readDecomp decomposition
    (_decimal, line7) = span (/= ';') (tail line6)
    (_digit, line8) = span (/= ';') (tail line7)
    (_numeric, line9) = span (/= ';') (tail line8)
    (_bidiM, line10) = span (/= ';') (tail line9)
    (_uni1Name, line11) = span (/= ';') (tail line10)
    (_iso, line12) = span (/= ';') (tail line11)
    (sUpper, line13) = span (/= ';') (tail line12)
    (sLower, line14) = span (/= ';') (tail line13)
    sTitle = tail line14

-------------------------------------------------------------------------------
-- Parse SpecialCasing.txt
-------------------------------------------------------------------------------

-- type SpecialCasings = Map.Map Char [SpecialCasing]
type SpecialCasings = Map.Map Char SpecialCasing

data SpecialCasing = SpecialCasing
    { _scChar       :: Char
    , _scLower      :: String
    , _scTitle      :: String
    , _scUpper      :: String
    -- , _scConditions :: [SpecialCasingCondition]
    }

parseSpecialCasingLines
    :: forall m. (Monad m)
    => SerialT m String
    -> m SpecialCasings
parseSpecialCasingLines
    = Stream.fold
        ( Fold.mapMaybe parseSpecialCasing
        $ Fold.foldl' combineSpecialCasings mempty
        )

    where

    -- combineSpecialCasings acc x = Map.insertWith (++) (_scChar x) [x] acc
    combineSpecialCasings acc sc = Map.insert (_scChar sc) sc acc

parseSpecialCasing :: String -> Maybe SpecialCasing
parseSpecialCasing line
    | null line        = Nothing
    | head line == '#' = Nothing
    -- Keep only entries without condititions
    | null conditions  = Just specialCasing
    | otherwise        = Nothing

    where

    (rawChar, line1) = span (/= ';') line
    char = readCodePoint rawChar
    (rawLower, line2) = span (/= ';') (tail line1)
    lower = toChars rawLower
    (rawTitle, line3) = span (/= ';') (tail line2)
    title = toChars rawTitle
    (rawUpper, line4) = span (/= ';') (tail line3)
    upper = toChars rawUpper
    (rawConditions, _line5) = span (/= ';') (tail line4)
    (rawConditions', _comment) = span (/= '#') rawConditions
    conditions = words (trim' rawConditions')
    specialCasing = SpecialCasing
        { _scChar  = char
        , _scLower = lower
        , _scTitle = title
        , _scUpper = upper
        -- , _scConditions = conditions
        }

    toChars = fmap readCodePoint . words

-------------------------------------------------------------------------------
-- Parsing CaseFolding.txt
-------------------------------------------------------------------------------

data CaseFoldingType
    = CommonCaseFolding
    | FullCaseFolding
    | SimpleCaseFolding
    | SpecialCaseFolding
    deriving (Eq, Ord)

type CaseFoldings = (Char, [(CaseFoldingType, String)])
type CaseFoldingLine = (Char, CaseFoldingType, String)

parseCaseFoldingLines
    :: forall t m. (IsStream t, Monad m)
    => t m String
    -> t m CaseFoldings
parseCaseFoldingLines
    = Stream.groupsBy sameChar combine
    . Stream.mapMaybe parseCaseFoldingLine

    where

    sameChar (c1, _, _) (c2, _, _) = c1 == c2

    combine = Fold.foldr
        (\(c, ty, cs) (_, xs) -> (c, (ty, cs):xs))
        ('\0', mempty)

parseCaseFoldingLine :: String -> Maybe CaseFoldingLine
parseCaseFoldingLine line
    | null line        = Nothing
    | head line == '#' = Nothing
    | otherwise        = Just (char, caseFoldingType, caseFolding)

    where

    (rawChar, line1) = span (/= ';') line
    char = readCodePoint rawChar
    (rawCaseFoldType, line2) = span (/= ';') (tail line1)
    caseFoldingType = case trim rawCaseFoldType of
        "C" -> CommonCaseFolding
        "F" -> FullCaseFolding
        "S" -> SimpleCaseFolding
        "T" -> SpecialCaseFolding
        ty  -> error ("Unsupported case folding type: " <> ty)
    (rawCaseFolding, _) = span (/= ';') (tail line2)
    caseFolding = toChars rawCaseFolding

    toChars = fmap readCodePoint . words

-------------------------------------------------------------------------------
-- Parsing DerivedNumericValues.txt
-------------------------------------------------------------------------------

type NumericValue = Either Int Rational
type CharNumericValue = (Char, NumericValue)
type CharRangeNumericValue = (Char, Char, NumericValue)
type DerivedNumericValuesLine = Either CharNumericValue CharRangeNumericValue

parseDerivedNumericValuesLine :: String -> Maybe DerivedNumericValuesLine
parseDerivedNumericValuesLine line
    | null line        = Nothing
    | head line == '#' = Nothing
    | otherwise        =
        let (range  , line1) = span (/= ';') line
            (_field1, line2) = span (/= ';') (tail line1)
            (_field2, line3) = span (/= ';') (tail line2)
            value            = takeWhile (/= '#') (tail line3)
            value' = parseValue (trim' value)
        in Just (bimap (,value') (mkRange value') (parseCodePointRange range))

    where

    mkRange :: NumericValue -> (Char, Char) -> CharRangeNumericValue
    mkRange value (c1, c2) = (c1, c2, value)

    parseValue :: String -> NumericValue
    parseValue raw =
        let (numerator, denominator) = span (/= '/') raw
        in if null denominator
            then Left  (read numerator)
            else Right (read numerator % read (tail denominator))

parseDerivedNumericValuesLines
    :: (IsStream t, Monad m)
    => t m String
    -> t m CharNumericValue
parseDerivedNumericValuesLines
    = Stream.unfoldMany (Unfold.unfoldr mkCharNumericValue)
    . Stream.mapMaybe parseDerivedNumericValuesLine

    where

    mkCharNumericValue
        :: DerivedNumericValuesLine
        -> Maybe (CharNumericValue, DerivedNumericValuesLine)
    mkCharNumericValue = \case
        Left charValue -> Just (charValue, Right ((fst charValue),  '\0', Left 0))
        Right (c1, c2, value) -> if c1 <= c2
            then Just ((c1, value), Right (succ c1, c2, value))
            else Nothing

-------------------------------------------------------------------------------
-- Parsing Aliases
-------------------------------------------------------------------------------

data AliasType
    = Correction
    | Control
    | Alternate
    | Figment
    | Abbreviation
    deriving (Eq, Ord, Read, Show)

newtype Alias = Alias String
instance Show Alias where
    show (Alias s) = "Ptr \"" <> s <> "\\0\"#"

type Aliases = [(AliasType, Alias)]
data CharAliases = CharAliases
    { _aChar    :: !Char
    , _aAliases :: !Aliases }
type AliasesLine = (Char, Alias, AliasType)

parseAliasesLine :: String -> Maybe AliasesLine
parseAliasesLine line
    | null line        = Nothing
    | head line == '#' = Nothing
    | otherwise        =
        let (char , line1) = span (/= ';') line
            (alias, line2) = span (/= ';') (tail line1)
            type_          = tail line2
        in Just (readCodePoint char, Alias alias, readAliasType type_)

    where

    readAliasType :: String -> AliasType
    readAliasType a = read (toUpper (head a) : tail a)


parseAliasesLines :: (IsStream t, Monad m) => t m String -> t m CharAliases
parseAliasesLines
    = Stream.groupsBy compareChar
        (Fold.foldr combineAliases (CharAliases '\0' mempty))
    . Stream.mapMaybe parseAliasesLine

    where

    compareChar :: AliasesLine -> AliasesLine -> Bool
    compareChar (char1, _, _) (char2, _, _) = char1 == char2

    combineAliases :: AliasesLine -> CharAliases -> CharAliases
    combineAliases (char, alias, type_) (CharAliases _ as) =
        CharAliases char ((type_, alias):as)

-------------------------------------------------------------------------------
-- Parsing Names
-------------------------------------------------------------------------------

data CharName = CharName
    { _nChar    :: !Char
    , _nName    :: !String }

type CharRangeName = (Char, Char, String)
type DerivedNameLine = Either CharName CharRangeName

parseDerivedNameLine :: String -> Maybe DerivedNameLine
parseDerivedNameLine line
    | null line        = Nothing
    | head line == '#' = Nothing
    | otherwise        =
        let (range, line1) = span (/= ';') line
            name = trim' (tail line1)
        in Just (bimap (`CharName` name) (mkRange name) (parseCodePointRange range))
    where

    mkRange :: String -> (Char, Char) -> CharRangeName
    mkRange name (c1, c2) = case elemIndex '*' name of
        Nothing -> error
            $ "Range name should contain “*”: "
            <> show (showHexCodepoint c1, showHexCodepoint c2, name)
        Just k  -> if k == length name - 1
            then (c1, c2, init name)
            else error
                $ "Unexpected “*” before the end of range name: "
                <> show (showHexCodepoint c1, showHexCodepoint c2, name)

parseDerivedNameLines
    :: forall t m. (IsStream t, Monad m) =>
    t m String ->
    t m CharName
parseDerivedNameLines
    = Stream.unfoldMany (Unfold.unfoldr mkCharsNames)
    . Stream.mapMaybe parseDerivedNameLine

    where

    mkCharsNames :: DerivedNameLine -> Maybe (CharName, DerivedNameLine)
    mkCharsNames = \case
        Left named -> Just (named, Right ((_nChar named),  '\0', mempty))
        Right (c1, c2, template) -> if c1 <= c2
            then Just ( mkName template c1
                      , Right (succ c1, c2, template) )
            else Nothing

    mkName :: String -> Char -> CharName
    mkName template char = CharName
        { _nChar = char
        , _nName = template <> fmap toUpper (showHexCodepoint char) }

-------------------------------------------------------------------------------
-- Parsing Identifier_Status
-------------------------------------------------------------------------------

data IdentifierStatus = Restricted | Allowed
    deriving (Eq, Show, Read)

data CharIdentifierStatus = CharIdentifierStatus
    { _idStatusChar :: !Char
    , _idStatus     :: !IdentifierStatus }
    deriving (Show)

type IdentifierStatusLine = (Char, Char, IdentifierStatus)

parseIdentifierStatusLine :: String -> Maybe IdentifierStatusLine
parseIdentifierStatusLine = \case
    ""         -> Nothing -- empty line
    '#':_      -> Nothing -- comment
    '\xFEFF':_ -> Nothing -- BOM
    line       ->
        let (rawRange, line1) = span (/= ';') line
            line2 = takeWhile (/= '#') (tail line1)
            range = parseCodePointRange rawRange
            status = read (trim' (tail line2))
        in Just (either (mkRange status . (id &&& id)) (mkRange status) range)

    where

    mkRange :: IdentifierStatus -> (Char, Char) -> IdentifierStatusLine
    mkRange status (c1, c2) = (c1, c2, status)

parseIdentifierStatusLines
    :: forall t m. (IsStream t, Monad m)
    => t m String
    -> t m CharIdentifierStatus
parseIdentifierStatusLines
    = Stream.unfoldMany (Unfold.unfoldr mkIdentifiersStatus)
    . Stream.mapMaybe parseIdentifierStatusLine

    where

    mkIdentifiersStatus
        :: IdentifierStatusLine
        -> Maybe (CharIdentifierStatus, IdentifierStatusLine)
    mkIdentifiersStatus (c1, c2, status) = if c1 <= c2
        then Just (CharIdentifierStatus c1 status, (succ c1, c2, status))
        else Nothing

-------------------------------------------------------------------------------
-- Parsing Identifier_Type
-------------------------------------------------------------------------------

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
    deriving (Eq, Ord, Bounded, Enum, Show, Read)

data CharIdentifierTypes = CharIdentifierTypes
    { _idTypesChar :: !Char
    , _idTypes     :: ![IdentifierType] }
    deriving (Show)

type IdentifierTypeLine = (Char, Char, [IdentifierType])

parseIdentifierTypeLine :: String -> Maybe IdentifierTypeLine
parseIdentifierTypeLine = \case
    ""         -> Nothing -- empty line
    '#':_      -> Nothing -- comment
    '\xFEFF':_ -> Nothing -- BOM
    line       ->
        let (rawRange, line1) = span (/= ';') line
            line2 = takeWhile (/= '#') (tail line1)
            range = parseCodePointRange rawRange
            types = read <$> words (trim' (tail line2))
        in Just (either (mkRange types . (id &&& id)) (mkRange types) range)

    where

    mkRange :: [IdentifierType] -> (Char, Char) -> IdentifierTypeLine
    mkRange type_ (c1, c2) = (c1, c2, type_)

parseIdentifierTypeLines
    :: forall t m. (IsStream t, Monad m)
    => t m String
    -> t m CharIdentifierTypes
parseIdentifierTypeLines
    = Stream.unfoldMany (Unfold.unfoldr mkIdentifiersStatus)
    . Stream.mapMaybe parseIdentifierTypeLine

    where

    mkIdentifiersStatus
        :: IdentifierTypeLine
        -> Maybe (CharIdentifierTypes, IdentifierTypeLine)
    mkIdentifiersStatus (c1, c2, types) = if c1 <= c2
        then Just (CharIdentifierTypes c1 types, (succ c1, c2, types))
        else Nothing

-------------------------------------------------------------------------------
-- Parsing Confusables
-------------------------------------------------------------------------------

data Confusable = Confusable
    { _confusableChar     :: !Char
    , _confusablePrototype :: !String }
    deriving (Eq, Ord, Show)

parseConfusablesLine :: String -> Maybe Confusable
parseConfusablesLine = \case
    ""         -> Nothing -- empty line
    '#':_      -> Nothing -- comment
    '\xFEFF':_ -> Nothing -- BOM
    line       ->
        let (rawChar, line1) = span (/= ';') line
            line2 = takeWhile (/= ';') (tail line1)
            char = readCodePoint rawChar
            prototype = readCodePoint <$> words (trim' (tail line2))
        in Just (Confusable char prototype)

parseConfusablesLines
    :: forall t m. (IsStream t, Monad m)
    => t m String
    -> t m Confusable
parseConfusablesLines
    = Stream.mapMaybe parseConfusablesLine

-------------------------------------------------------------------------------
-- Parsing Intentional Confusables
-------------------------------------------------------------------------------

data IntentionalConfusable = IntentionalConfusable
    { _intantionConfusableChar      :: !Char
    , _intantionConfusablePrototype :: !Char }
    deriving (Eq, Ord, Show)

parseIntentionalConfusablesLine :: String -> Maybe IntentionalConfusable
parseIntentionalConfusablesLine = \case
    ""         -> Nothing -- empty line
    '#':_      -> Nothing -- comment
    '\xFEFF':_ -> Nothing -- BOM
    line       ->
        let (rawChar, line1) = span (/= ';') line
            line2 = takeWhile (/= '#') (tail line1)
            char = readCodePoint rawChar
            prototype = readCodePoint (trim' (tail line2))
        in Just (IntentionalConfusable char prototype)

parseIntentionalConfusablesLines
    :: forall t m. (IsStream t, Monad m)
    => t m String
    -> t m IntentionalConfusable
parseIntentionalConfusablesLines
    = Stream.mapMaybe parseIntentionalConfusablesLine

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

readLinesFromFile :: String -> SerialT IO String
readLinesFromFile file =
    withFile file Sys.ReadMode
        $ \h ->
              Stream.unfold Handle.read h & Unicode.decodeUtf8
                  & unicodeLines Fold.toList

    where

    unicodeLines = Stream.splitOnSuffix (== '\n')

    withFile file_ mode =
        Stream.bracket (liftIO $ Sys.openFile file_ mode) (liftIO . Sys.hClose)


moduleToFileName :: String -> String
moduleToFileName = map (\x -> if x == '.' then '/' else x)

dirFromFileName :: String -> String
dirFromFileName = reverse . dropWhile (/= '/') . reverse

-- ModuleRecipe is a tuple of the module name and a function that generates the
-- module using the module name
type ModuleRecipe a = (String, String -> Fold IO a String)

-- GeneratorRecipe is a list of ModuleRecipe
type GeneratorRecipe a = [ModuleRecipe a]

fileEmitter :: String -> String -> ModuleRecipe a -> Fold IO a ()
fileEmitter file outdir (modName, fldGen) = Fold.rmapM action $ fldGen modName

    where

    pretext version = mconcat
        [ "-- autogenerated from https://www.unicode.org/Public/"
        , version
        , "/ucd/"
        , file
        ,"\n"
        ]
    outfile = outdir <> moduleToFileName modName <> ".hs"
    outfiledir = dirFromFileName outfile
    action c = do
        version <-
            catch
                (getEnv "UNICODE_VERSION")
                (\(_ :: IOException) -> return "<unknown>")
        createDirectoryIfMissing True outfiledir
        writeFile outfile (pretext version ++ c)

runGenerator ::
       String
    -> String
    -> (SerialT IO String -> SerialT IO a)
    -> String
    -> GeneratorRecipe a
    -> IO ()
runGenerator indir file transformLines outdir recipes =
    readLinesFromFile (indir </> file) & transformLines & Stream.fold combinedFld

    where

    generatedFolds = map (fileEmitter file outdir) recipes
    combinedFld = void $ Fold.distribute generatedFolds

genCoreModules :: String -> String -> [String] -> IO ()
genCoreModules indir outdir props = do

    compExclu <-
        readLinesFromFile (indir </> "DerivedNormalizationProps.txt")
            & parsePropertyLines
            & Stream.find (\(name, _) -> name == "Full_Composition_Exclusion")
            & fmap (snd . fromMaybe ("", []))

    non0CC <-
        readLinesFromFile (indir </> "extracted/DerivedCombiningClass.txt")
            & parsePropertyLines
            & Stream.filter (\(name, _) -> name /= "0")
            & Stream.map snd
            & Stream.fold (Fold.foldl' (++) [])

    specialCasings <-
        readLinesFromFile (indir </> "SpecialCasing.txt")
            & parseSpecialCasingLines

    runGenerator
        indir
        "Blocks.txt"
        parseBlockLines
        outdir
        [ blocks ]

    runGenerator
        indir
        "UnicodeData.txt"
        parseUnicodeDataLines
        outdir
        [ compositions compExclu non0CC
        , combiningClass
        , decomposable
        , decomposableK
        , decompositions
        , decompositionsK2
        , decompositionsK
        , generalCategory
        , simpleUpperCaseMapping
        , simpleLowerCaseMapping
        , simpleTitleCaseMapping
        , specialUpperCaseMapping specialCasings
        , specialLowerCaseMapping specialCasings
        , specialTitleCaseMapping specialCasings
        ]

    runGenerator
        indir
        "PropList.txt"
        parsePropertyLines
        outdir
        [ propList ]

    runGenerator
        indir
        "DerivedCoreProperties.txt"
        parsePropertyLines
        outdir
        [ derivedCoreProperties ]

    runGenerator
        indir
        "extracted/DerivedNumericValues.txt"
        parseDerivedNumericValuesLines
        outdir
        [ derivedNumericValues ]

    runGenerator
        indir
        "CaseFolding.txt"
        parseCaseFoldingLines
        outdir
        [ caseFolding ]

    where

    blocks =
        ( "Unicode.Internal.Char.Blocks"
        , genBlocksModule)

    propList =
        ("Unicode.Internal.Char.PropList"
        , (`genCorePropertiesModule` (`elem` props)))

    derivedCoreProperties =
        ("Unicode.Internal.Char.DerivedCoreProperties"
        , (`genCorePropertiesModule` (`elem` props)))

    compositions exc non0 =
        ( "Unicode.Internal.Char.UnicodeData.Compositions"
        , \m -> genCompositionsModule m exc non0)

    combiningClass =
        ( "Unicode.Internal.Char.UnicodeData.CombiningClass"
        , genCombiningClassModule)

    decomposable =
        ( "Unicode.Internal.Char.UnicodeData.Decomposable"
        , (`genDecomposableModule` Canonical))

    decomposableK =
        ( "Unicode.Internal.Char.UnicodeData.DecomposableK"
        , (`genDecomposableModule` Kompat))

    decompositions =
        let post = ["  c -> [c]"]
        in  ( "Unicode.Internal.Char.UnicodeData.Decompositions"
            , \m -> genDecomposeDefModule m [] post Canonical (const True))

    decompositionsK2 =
        let post = ["  c -> [c]"]
        in  ( "Unicode.Internal.Char.UnicodeData.DecompositionsK2"
            , \m -> genDecomposeDefModule m [] post Kompat (>= 60000))

    decompositionsK =
        let pre = ["import qualified " <> fst decompositionsK2 <> " as DK2", ""]
            post = ["  c -> DK2.decompose c"]
        in  ( "Unicode.Internal.Char.UnicodeData.DecompositionsK"
            , \m -> genDecomposeDefModule m pre post Kompat (< 60000))

    generalCategory =
         ( "Unicode.Internal.Char.UnicodeData.GeneralCategory"
         , genGeneralCategoryModule)

    simpleUpperCaseMapping =
         ( "Unicode.Internal.Char.UnicodeData.SimpleUpperCaseMapping"
         , \m -> genSimpleCaseMappingModule m "toSimpleUpperCase" _simpleUpperCaseMapping)

    simpleLowerCaseMapping =
         ( "Unicode.Internal.Char.UnicodeData.SimpleLowerCaseMapping"
         , \m -> genSimpleCaseMappingModule m "toSimpleLowerCase" _simpleLowerCaseMapping)

    simpleTitleCaseMapping =
         ( "Unicode.Internal.Char.UnicodeData.SimpleTitleCaseMapping"
         , \m -> genSimpleCaseMappingModule m "toSimpleTitleCase" _simpleTitleCaseMapping)

    specialUpperCaseMapping sc =
         ( "Unicode.Internal.Char.SpecialCasing.UpperCaseMapping"
         , \m -> genSpecialCaseMappingModule m
                    "toSpecialUpperCase"
                    sc
                    _scUpper
                    _simpleUpperCaseMapping )

    specialLowerCaseMapping sc =
         ( "Unicode.Internal.Char.SpecialCasing.LowerCaseMapping"
         , \m -> genSpecialCaseMappingModule m
                    "toSpecialLowerCase"
                    sc
                    _scLower
                    _simpleLowerCaseMapping )

    specialTitleCaseMapping sc =
         ( "Unicode.Internal.Char.SpecialCasing.TitleCaseMapping"
         , \m -> genSpecialCaseMappingModule m
                    "toSpecialTitleCase"
                    sc
                    _scTitle
                    _simpleTitleCaseMapping )

    caseFolding =
         ( "Unicode.Internal.Char.CaseFolding"
         , genCaseFolding )

    derivedNumericValues =
         ( "Unicode.Internal.Char.DerivedNumericValues"
         , genNumericValuesModule )

genNamesModules :: String -> String -> IO ()
genNamesModules indir outdir = do
    runGenerator
        indir
        "extracted/DerivedName.txt"
        parseDerivedNameLines
        outdir
        [names]

    runGenerator
        indir
        "NameAliases.txt"
        parseAliasesLines
        outdir
        [aliases]

    where

    names =
         ( "Unicode.Internal.Char.UnicodeData.DerivedName"
         , genNamesModule )
    aliases =
         ( "Unicode.Internal.Char.UnicodeData.NameAliases"
         , genAliasesModule )

genScriptsModules :: String -> String -> IO ()
genScriptsModules indir outdir = do
    scriptAliases <-
        readLinesFromFile (indir </> "PropertyValueAliases.txt")
            & parsePropertyValueAliasesLines
            & Stream.lookup "sc"
            & fmap (fromMaybe mempty)

    extensions <-
        readLinesFromFile (indir </> "ScriptExtensions.txt")
            & parseScriptExtensionsLines
            & Stream.foldr
                (\(CharScriptExtensions c s) -> Map.insertWith (<>) c s)
                mempty

    runGenerator
        indir
        "Scripts.txt"
        parseScriptLines
        outdir
        [ scripts scriptAliases
        , scriptExtensions scriptAliases extensions ]

    where

    scripts scriptAliases =
        ( "Unicode.Internal.Char.Scripts"
        , \m -> genScriptsModule m scriptAliases )

    scriptExtensions scriptAliases extensions =
        ( "Unicode.Internal.Char.ScriptExtensions"
        , \m -> genScriptExtensionsModule m scriptAliases extensions )

genSecurityModules :: String -> String -> IO ()
genSecurityModules indir outdir = do
    runGenerator
        indir
        "IdentifierStatus.txt"
        parseIdentifierStatusLines
        outdir
        [isAllowedInIdentifier]

    runGenerator
        indir
        "IdentifierType.txt"
        parseIdentifierTypeLines
        outdir
        [identifierTypes]

    runGenerator
        indir
        "confusables.txt"
        parseConfusablesLines
        outdir
        [confusables]

    runGenerator
        indir
        "intentional.txt"
        parseIntentionalConfusablesLines
        outdir
        [intentional]

    where

    isAllowedInIdentifier =
         ( "Unicode.Internal.Char.Security.IdentifierStatus"
         , genIdentifierStatusModule )

    identifierTypes =
         ( "Unicode.Internal.Char.Security.IdentifierType"
         , genIdentifierTypeModule )

    confusables =
         ( "Unicode.Internal.Char.Security.Confusables"
         , genConfusablesModule )

    intentional =
         ( "Unicode.Internal.Char.Security.IntentionalConfusables"
         , genIntentionalConfusablesModule )
