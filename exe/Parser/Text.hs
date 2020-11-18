-- |
-- Module      : Parser.Text
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
--               (c) Harendra Kumar
--               (c) 2014-2015 Antonio Nikishaev
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental

-- The original unicode database parser was taken from
-- https://github.com/composewell/unicode-transforms but was completely
-- rewritten from scratch to parse from UCD text files instead of XML, only
-- some types remain the same. That code in turn was originally taken from
-- https://github.com/llelf/prose (Antonio Nikishaev) and heavily modified by
-- Harendra Kumar.
--
module Parser.Text (genModules) where

import Control.Monad (void)
import Data.Bits (shiftL)
import Data.Char (chr, ord, isSpace)
import Data.Function ((&))
import Data.List (unfoldr, isInfixOf, intersperse)
import Data.Maybe (fromMaybe)
import Streamly.Internal.Data.Fold (Fold(..))
import System.Directory (createDirectoryIfMissing)

import qualified Data.Set as Set
import qualified Streamly.Prelude as Stream
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Unicode.Stream as Unicode
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified System.IO as Sys


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
    deriving (Show, Read)

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
        }
    deriving (Show)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

apacheLicense :: String -> String
apacheLicense modName =
    unlines
        [ "-- |"
        , "-- Module      : " ++ modName
        , "-- Copyright   : (c) 2020 Composewell Technologies and Contributors"
        , "-- License     : Apache-2.0"
        , "-- Maintainer  : streamly@composewell.com"
        , "-- Stability   : experimental"
        ]

readCodePoint :: String -> Char
readCodePoint = chr . read . ("0x"++)

genSignature :: String -> String
genSignature testBit = testBit <> " :: Char -> Bool"

-- | Check that var is between minimum and maximum of orderList
genRangeCheck :: String -> [Int] -> String
genRangeCheck var ordList =
      var <> " >= "
      <> show (minimum ordList) <> " && " <> var <> " <= "
      <> show (maximum ordList)

genBitmap :: String -> [Int] -> String
genBitmap funcName ordList = unlines
  [ "{-# INLINE " ++ funcName  ++ " #-}"
  , genSignature funcName
  , funcName
        <> " = \\c -> let n = ord c in "
        ++ genRangeCheck "n" ordList
        ++ " && lookupBit64 bitmap# n"
  , "  where"
  , "    bitmap# = "
        ++ show (bitMapToAddrLiteral (positionsToBitMap ordList))
        ++ "#"
  ]

positionsToBitMap :: [Int] -> [Bool]
positionsToBitMap = go 0
  where
    go _ [] = []
    go i xxs@(x : xs)
      | i < x     = False : go (i + 1) xxs
      | otherwise = True  : go (i + 1) xs

bitMapToAddrLiteral :: [Bool] -> String
bitMapToAddrLiteral = map (chr . toByte . padTo8) . unfoldr go
  where
    go :: [a] -> Maybe ([a], [a])
    go [] = Nothing
    go xs = Just $ splitAt 8 xs

    padTo8 :: [Bool] -> [Bool]
    padTo8 xs
      | length xs >= 8 = xs
      | otherwise = xs ++ replicate (8 - length xs) False

    toByte :: [Bool] -> Int
    toByte xs = sum $ map (\i -> if xs !! i then 1 `shiftL` i else 0) [0..7]


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
-- Parser
-------------------------------------------------------------------------------

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
filterNonHangul = Fold.lfilter (not . isHangul . _char)

filterDecomposableType ::
       Monad m => DType -> Fold m DetailedChar a -> Fold m DetailedChar a
filterDecomposableType dtype =
    Fold.lfilter ((/= DCSelf) . _decomposition)
      . Fold.lfilter (predicate . _decompositionType)

    where

    predicate =
        case dtype of
            Canonical -> (== Just DTCanonical)
            Kompat -> const True

genDecomposableModule ::
       Monad m => String -> DType -> Fold m DetailedChar String
genDecomposableModule moduleName dtype =
    filterNonHangul
        $ filterDecomposableType dtype
        $ Fold.Fold step initial done

    where

    initial = return []

    step st a = return $ ord (_char a) : st

    done st =
        return
          $ unlines
                [ apacheLicense moduleName
                , "-- autogenerated from Unicode data"
                , "module " <> moduleName
                , "(isDecomposable)"
                , "where"
                , ""
                , "import Data.Char (ord)"
                , "import Data.Unicode.Internal.Bits (lookupBit64)"
                , ""
                , genBitmap "isDecomposable" (reverse st)
                ]

genCombiningClassModule :: Monad m => String -> Fold m DetailedChar String
genCombiningClassModule moduleName =
    Fold.lfilter (\dc -> _combiningClass dc /= 0) $ Fold step initial done

    where

    initial = return ([], [])

    step (st1, st2) a =
        return (genCombiningClassDef a : st1, ord (_char a) : st2)

    done (st1, st2) =
        return
          $ unlines
                [ apacheLicense moduleName
                , "-- autogenerated from Unicode data"
                , "module " <> moduleName
                , "(getCombiningClass, isCombining)"
                , "where"
                , ""
                , "import Data.Char (ord)"
                , "import Data.Unicode.Internal.Bits (lookupBit64)"
                , ""
                , "getCombiningClass :: Char -> Int"
                , unlines (reverse st1)
                , "getCombiningClass _ = 0\n"
                , ""
                , genBitmap "isCombining" (reverse st2)
                ]

    genCombiningClassDef dc =
        "getCombiningClass "
          <> show (_char dc) <> " = " <> show (_combiningClass dc)

genDecomposeDefModule ::
       Monad m
    => String
    -> [String]
    -> [String]
    -> DType
    -> (Int -> Bool)
    -> Fold m DetailedChar String
genDecomposeDefModule moduleName before after dtype pred =
    Fold.lfilter (pred . ord . _char)
      $ filterNonHangul
      $ filterDecomposableType dtype $ Fold.Fold step initial done

    where

    decomposeChar c DCSelf = [c]
    decomposeChar _c (DC ds) = ds

    genHeader =
        [ apacheLicense moduleName
        , "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}"
        , "-- autogenerated from Unicode data"
        , "module " <> moduleName
        , "(decomposeChar)"
        , "where"
        , ""
        ]
    genSign =
        [ "-- Note: this is a partial function we do not expect to call"
        , "-- this if isDecomposable returns false."
        , "{-# NOINLINE decomposeChar #-}"
        , "decomposeChar :: Char -> [Char]"
        ]
    initial = return []

    step st dc = return $ genDecomposeDef dc : st

    done st =
        let body = genHeader ++ before ++ genSign ++ reverse st ++ after
         in return $ unlines body

    genDecomposeDef dc =
        "decomposeChar "
          <> show (_char dc)
          <> " = " <> show (decomposeChar (_char dc) (_decomposition dc))

genCompositionsModule ::
       Monad m
    => String
    -> [Int]
    -> [Int]
    -> Fold m DetailedChar String
genCompositionsModule moduleName compExclu non0CC =
    Fold.lfilter (not . flip elem compExclu . ord . _char)
      $ filterNonHangul
      $ Fold.lfilter (isDecompositionLen2 . _decomposition)
      $ filterDecomposableType Canonical $ Fold.Fold step initial done

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

    initial = return ([], [], [])

    step (dec, sp, ss) dc = return (dec1, sp1, ss1)

        where

        d01 = decompPair dc
        d1Ord = ord $ d01 !! 1
        dec1 = genComposePairDef "composePair" dc : dec
        sp1 =
            if d1Ord `notElem` non0CC
            then genComposePairDef "composeStarterPair" dc : sp
            else sp
        ss1 =
            if d1Ord `notElem` non0CC
            then d1Ord : ss
            else ss

    header =
        [ apacheLicense moduleName
        , "-- autogenerated from Unicode data"
        , "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}"
        , "module " <> moduleName
        , "(composePair, composeStarterPair, isSecondStarter)"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Data.Unicode.Internal.Bits (lookupBit64)"
        , ""
        ]

    composePair decomps =
        [ "{-# NOINLINE composePair #-}"
        , "composePair :: Char -> Char -> Maybe Char"
        , unlines decomps
        , "composePair _ _ = " <> "Nothing" <> "\n"
        , ""
        ]

    composeStarterPair starterPairs =
        [ "composeStarterPair :: Char -> Char -> Maybe Char"
        , unlines starterPairs
        , "composeStarterPair _ _ = " <> "Nothing" <> "\n"
        , ""
        ]

    isSecondStarter secondStarters =
        [genBitmap "isSecondStarter" secondStarters]

    done (dec, sp, ss) =
        return
          $ unlines
          $ header
          ++ composePair (reverse dec)
          ++ composeStarterPair (reverse sp)
          ++ isSecondStarter (Set.toList (Set.fromList ss))

parseProperty :: Monad m => String -> Fold m String (String, [Int])
parseProperty propHeader = Fold step initial extract

    where

    initial = return Nothing

    extract Nothing = return ("", [])
    extract (Just x) = return x

    step Nothing str
        | propHeader `isInfixOf` str =
            return $ Just (extractPropertyName str, [])
        | otherwise = return Nothing
    step st [] = return st
    step st (x:_)
        | x == '#' = return st
    step (Just (name, ordList)) str =
        return $ Just (name, ordList ++ (parseRange . getRange) str)

    extractPropertyName :: String -> String
    extractPropertyName str =
        dropWhile (not . flip elem eqChars) str & tail & dropWhile isSpace
          & takeWhile (not . isSpace)

        where

        eqChars = [':', '=']

    getRange :: String -> String
    getRange = takeWhile (not . isSpace) . takeWhile (/= ';')

    parseRange :: String -> [Int]
    parseRange rng =
        if '.' `elem` rng
        then let low = read $ "0x" ++ takeWhile (/= '.') rng
                 high =
                     read $ "0x" ++ reverse (takeWhile (/= '.') (reverse rng))
              in [low .. high]
        else [read $ "0x" ++ rng]

genCorePropertiesModule ::
       Monad m => String -> [String] -> Fold m (String, [Int]) String
genCorePropertiesModule moduleName props =
    Fold.lfilter (\(name, _) -> name `elem` props) $ Fold step initial done

    where

    prop2FuncName x = "is" ++ x

    initial = return []

    step st (name, bits) = return $ genBitmap (prop2FuncName name) bits : st

    done st = return $ unlines $ header ++ st

    exportList = unwords $ intersperse "," $ map prop2FuncName props
    header =
        [ apacheLicense moduleName
        , "-- autogenerated from Unicode data"
        , "module " <> moduleName
        , "(" ++ exportList ++ ")"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Data.Unicode.Internal.Bits (lookupBit64)"
        ]

parseDetailedChar :: String -> DetailedChar
parseDetailedChar line =
    DetailedChar
        (readCodePoint char) name (read gc) (read combining) dctype dcval

    where

    (char, line1) = span (/= ';') line
    (name, line2) = span (/= ';') (tail line1)
    (gc, line3) = span (/= ';') (tail line2)
    (combining, line4) = span (/= ';') (tail line3)
    (_bidi, line5) = span (/= ';') (tail line4)
    (decomposition, line6) = span (/= ';') (tail line5)
    (dctype, dcval) = readDecomp decomposition
    (_numeric, line7) = span (/= ';') (tail line6)
    (_bidiM, line8) = span (/= ';') (tail line7)
    (_uni1Name, line9) = span (/= ';') (tail line8)
    (_iso, line10) = span (/= ';') (tail line9)
    (_sUpper, line11) = span (/= ';') (tail line10)
    (_sLower, line12) = span (/= ';') (tail line11)
    _sTitle = tail line12

genModules :: String -> String -> [String] -> IO ()
genModules indir outdir props = do
    unicodeDataH <- Sys.openFile unicodeData Sys.ReadMode
    derivedNormalizationPropsH <-
        Sys.openFile derivedNormalizationProps Sys.ReadMode
    derivedCorePropsH <- Sys.openFile derivedCoreProps Sys.ReadMode
    derivedCombiningClassH <- Sys.openFile derivedCombiningClass Sys.ReadMode
    compExclu <-
        readLinesFromHandle derivedNormalizationPropsH
          & Stream.splitOn isDivider (parseProperty "Derived Property:")
          & Stream.find (\(name, _) -> name == "Full_Composition_Exclusion")
          & fmap (fromMaybe ("", []))
          & fmap snd
    non0CC <-
        readLinesFromHandle derivedCombiningClassH
          & Stream.splitOn
                isDivider (parseProperty "Canonical_Combining_Class=")
          & Stream.filter (\(name, _) -> name /= "Not_Reordered")
          & Stream.map snd
          & Stream.fold (Fold.mkPureId (++) [])

    let compositions =
            ( "Data.Unicode.Properties.Compositions"
            , \m -> genCompositionsModule m compExclu non0CC )
        combiningClass =
            ( "Data.Unicode.Properties.CombiningClass"
            , genCombiningClassModule )
        decomposable =
            ("Data.Unicode.Properties.Decomposable"
            , \m -> genDecomposableModule m Canonical)
        decomposableK =
            ("Data.Unicode.Properties.DecomposableK"
            , \m -> genDecomposableModule m Kompat)
        decompositions =
            ( "Data.Unicode.Properties.Decompositions"
            , \m -> genDecomposeDefModule m [] [] Canonical (const True))
        decompositionsK2 =
            ( "Data.Unicode.Properties.DecompositionsK2"
            , \m -> genDecomposeDefModule m [] [] Kompat (>= 60000))
        decompositionsK =
            let pre = [ "import qualified " <> fst decompositionsK2 <> " as DK2"
                      , ""
                      ]
                post = ["decomposeChar c = DK2.decomposeChar c"]
             in ( "Data.Unicode.Properties.DecompositionsK"
                , \m -> genDecomposeDefModule m pre post Kompat (< 60000))
        core =
            ( "Data.Unicode.Properties.Core"
            , \m -> genCorePropertiesModule m props)
        unicodeDataFolds =
            [ compositions
            , combiningClass
            , decomposable
            , decomposableK
            , decompositions
            , decompositionsK2
            , decompositionsK
            ]
    -- XXX distribute_ does not work as expected, fixed in a later PR.
        combinedFold =
            void $ Fold.distribute (map emitFile unicodeDataFolds)
    readLinesFromHandle unicodeDataH & Stream.map parseDetailedChar
      & Stream.fold combinedFold
    readLinesFromHandle derivedCorePropsH
      & Stream.splitOn isDivider (parseProperty "Derived Property:")
      & Stream.fold (emitFile core)
    Sys.hClose unicodeDataH
    Sys.hClose derivedNormalizationPropsH
    Sys.hClose derivedCorePropsH
    Sys.hClose derivedCombiningClassH

    where


    readLinesFromHandle h =
        Stream.unfold Handle.read h
            & Unicode.decodeUtf8
            & Unicode.lines Fold.toList

    isDivider x = x == "# ================================================"

    unicodeData = indir <> "UnicodeData.txt"
    derivedCoreProps = indir <> "DerivedCoreProperties.txt"
    derivedNormalizationProps = indir <> "DerivedNormalizationProps.txt"
    derivedCombiningClass =
        indir <> "extracted/" <> "DerivedCombiningClass.txt"

    getFile = map (\x -> if x == '.' then '/' else x)
    getDir = reverse . dropWhile (/= '/') . reverse

    emitFile (modName, fldGen) =
        let fld = fldGen modName
            file = outdir <> getFile modName <> ".hs"
            dir = getDir file
            action c = createDirectoryIfMissing True dir >> writeFile file c
         in Fold.mapM action fld
