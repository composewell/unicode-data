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
    ) where

import Control.Applicative (Alternative(..))
import Control.Exception (catch, IOException)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bifunctor (Bifunctor(..))
import Data.Bits (Bits(..))
import Data.Char (chr, ord, isSpace, toUpper)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (dropWhileEnd, elemIndex, intersperse, sort, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Word (Word8)
import Numeric (showHex)
import Streamly.Data.Fold (Fold)
import Streamly.Prelude (IsStream, SerialT)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.FileSystem.Handle as Handle
import qualified System.IO as Sys
import qualified Streamly.Unicode.Stream as Unicode

import Prelude hiding (pred)

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

showHexCodepoint :: Char -> String
showHexCodepoint c =
    let hex = showHex (ord c) mempty
        padding = 4 - length hex
    in replicate padding '0' <> hex

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
genDecomposeDefModule moduleName before after dtype pred =
    Fold.filter (pred . ord . _char)
        $ filterNonHangul
        $ filterDecomposableType dtype $ done <$> Fold.foldl' step initial

    where

    decomposeChar c DCSelf = [c]
    decomposeChar _c (DC ds) = ds

    genHeader =
        [ apacheLicense 2020 moduleName
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(decompose)"
        , "where"
        , ""
        ]
    genSign =
        [ "-- Note: this is a partial function we do not expect to call"
        , "-- this if isDecomposable returns false."
        , "{-# NOINLINE decompose #-}"
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
        , "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}"
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
        in Just (bimap (,value') (mkRange value') (parseRange range))

    where

    parseRange :: String -> Either Char (Char, Char)
    parseRange
        = (\(c1, c2) -> maybe (Left c1) (Right . (c1,)) c2)
        . bimap readCodePoint (readCodePointM . drop 2)
        . span (/= '.')

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
        in Just (bimap (`CharName` name) (mkRange name) (parseRange range))
    where

    parseRange :: String -> Either Char (Char, Char)
    parseRange
        = (\(c1, c2) -> maybe (Left c1) (Right . (c1,)) c2)
        . bimap readCodePoint (readCodePointM . drop 2)
        . span (/= '.')

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
    readLinesFromFile (indir <> file) & transformLines & Stream.fold combinedFld

    where

    generatedFolds = map (fileEmitter file outdir) recipes
    combinedFld = void $ Fold.distribute generatedFolds

genCoreModules :: String -> String -> [String] -> IO ()
genCoreModules indir outdir props = do

    compExclu <-
        readLinesFromFile (indir <> "DerivedNormalizationProps.txt")
            & parsePropertyLines
            & Stream.find (\(name, _) -> name == "Full_Composition_Exclusion")
            & fmap (snd . fromMaybe ("", []))

    non0CC <-
        readLinesFromFile (indir <> "extracted/DerivedCombiningClass.txt")
            & parsePropertyLines
            & Stream.filter (\(name, _) -> name /= "0")
            & Stream.map snd
            & Stream.fold (Fold.foldl' (++) [])

    specialCasings <-
        readLinesFromFile (indir <> "SpecialCasing.txt")
            & parseSpecialCasingLines

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
        ( "Unicode.Internal.Char.UnicodeData.Decompositions"
        , \m -> genDecomposeDefModule m [] [] Canonical (const True))

    decompositionsK2 =
        ( "Unicode.Internal.Char.UnicodeData.DecompositionsK2"
        , \m -> genDecomposeDefModule m [] [] Kompat (>= 60000))

    decompositionsK =
        let pre = ["import qualified " <> fst decompositionsK2 <> " as DK2", ""]
            post = ["  c -> DK2.decompose c"]
         in ( "Unicode.Internal.Char.UnicodeData.DecompositionsK"
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
         , \m -> genNumericValuesModule m)

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
         , \m -> genNamesModule m )
    aliases =
         ( "Unicode.Internal.Char.UnicodeData.NameAliases"
         , \m -> genAliasesModule m )
