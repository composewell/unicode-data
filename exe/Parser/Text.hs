-- |
-- Module      : Parser.Text
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
--               (c) 2016-2017 Harendra Kumar
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

import Control.Exception (catch, IOException)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bits (shiftL)
import Data.Char (chr, ord, isSpace)
import Data.Function ((&))
import Data.List (unfoldr, intersperse)
import Data.Maybe (fromMaybe)
import Streamly.Data.Fold (Fold)
import Streamly.Prelude (IsStream, SerialT)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)

import qualified Data.Set as Set
import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
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
    var
        <> " >= "
        <> show (minimum ordList)
        <> " && " <> var <> " <= " <> show (maximum ordList)

genBitmap :: String -> [Int] -> String
genBitmap funcName ordList =
    unlines
        [ "{-# INLINE " ++ funcName ++ " #-}"
        , genSignature funcName
        , funcName <> " = \\c -> let n = ord c in "
              ++ genRangeCheck "n" ordList ++ " && lookupBit64 bitmap# n"
        , "  where"
        , "    bitmap# = "
              ++ show (bitMapToAddrLiteral (positionsToBitMap ordList)) ++ "#"
        ]

positionsToBitMap :: [Int] -> [Bool]
positionsToBitMap = go 0

    where

    go _ [] = []
    go i xxs@(x:xs)
        | i < x = False : go (i + 1) xxs
        | otherwise = True : go (i + 1) xs

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
-- Parsers
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Parsing UnicodeData.txt
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
            [ apacheLicense moduleName
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
            [ apacheLicense moduleName
            , "module " <> moduleName
            , "(combiningClass, isCombining)"
            , "where"
            , ""
            , "import Data.Char (ord)"
            , "import Unicode.Internal.Bits (lookupBit64)"
            , ""
            , "combiningClass :: Char -> Int"
            , unlines (reverse st1)
            , "combiningClass _ = 0\n"
            , ""
            , genBitmap "isCombining" (reverse st2)
            ]

    genCombiningClassDef dc =
        "combiningClass "
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
    Fold.filter (pred . ord . _char)
        $ filterNonHangul
        $ filterDecomposableType dtype $ done <$> Fold.foldl' step initial

    where

    decomposeChar c DCSelf = [c]
    decomposeChar _c (DC ds) = ds

    genHeader =
        [ apacheLicense moduleName
        , "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}"
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
        ]
    initial = []

    step st dc = genDecomposeDef dc : st

    done st =
        let body = genHeader ++ before ++ genSign ++ reverse st ++ after
         in unlines body

    genDecomposeDef dc =
        "decompose "
            <> show (_char dc)
            <> " = " <> show (decomposeChar (_char dc) (_decomposition dc))

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
        [ apacheLicense moduleName
        , "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}"
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
        [ apacheLicense moduleName
        , "module " <> moduleName
        , "(" ++ unwords (intersperse "," (map prop2FuncName exports)) ++ ")"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Unicode.Internal.Bits (lookupBit64)"
        ]

-------------------------------------------------------------------------------
-- Parsing property files
-------------------------------------------------------------------------------

trim :: String -> String
trim = takeWhile (not . isSpace) . dropWhile isSpace

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

    pretext version =
        "-- autogenerated from https://www.unicode.org/Public/"
            ++ version ++ "/ucd/" ++ file ++ "\n"
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

genModules :: String -> String -> [String] -> IO ()
genModules indir outdir props = do

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

    runGenerator
        indir
        "UnicodeData.txt"
        (Stream.map parseDetailedChar)
        outdir
        [ compositions compExclu non0CC
        , combiningClass
        , decomposable
        , decomposableK
        , decompositions
        , decompositionsK2
        , decompositionsK
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

    where

    propList =
        ("Unicode.Internal.Generated.PropList"
        , (`genCorePropertiesModule` (`elem` props)))

    derivedCoreProperties =
        ("Unicode.Internal.Generated.DerivedCoreProperties"
        , (`genCorePropertiesModule` (`elem` props)))

    compositions exc non0 =
        ( "Unicode.Internal.Generated.UnicodeData.Compositions"
        , \m -> genCompositionsModule m exc non0)

    combiningClass =
        ( "Unicode.Internal.Generated.UnicodeData.CombiningClass"
        , genCombiningClassModule)

    decomposable =
        ( "Unicode.Internal.Generated.UnicodeData.Decomposable"
        , (`genDecomposableModule` Canonical))

    decomposableK =
        ( "Unicode.Internal.Generated.UnicodeData.DecomposableK"
        , (`genDecomposableModule` Kompat))

    decompositions =
        ( "Unicode.Internal.Generated.UnicodeData.Decompositions"
        , \m -> genDecomposeDefModule m [] [] Canonical (const True))

    decompositionsK2 =
        ( "Unicode.Internal.Generated.UnicodeData.DecompositionsK2"
        , \m -> genDecomposeDefModule m [] [] Kompat (>= 60000))

    decompositionsK =
        let pre = ["import qualified " <> fst decompositionsK2 <> " as DK2", ""]
            post = ["decompose c = DK2.decompose c"]
         in ( "Unicode.Internal.Generated.UnicodeData.DecompositionsK"
            , \m -> genDecomposeDefModule m pre post Kompat (< 60000))
