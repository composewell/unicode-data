-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Generator
    ( -- * Recipe
      FileRecipe(..)
      -- * Generator
    , runGenerator
    , moduleToFileName
      -- * Bitmap
    , genBitmap
    , genEnumBitmap
    , bitMapToAddrLiteral
    , enumMapToAddrLiteral
    , chunkAddrLiteral
    , word32ToWord8s
    , splitPlanes
      -- * Helpers
    , unlinesBB
    , unwordsBB
    , apacheLicense
    ) where
import Data.Bits (Bits(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import qualified Data.List as L
import Data.Word (Word8, Word32)
import Data.Version (Version, showVersion)
import GHC.Stack (HasCallStack)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))

import UCD2Haskell.Common (Fold, showPaddedHeXB, showB, distribute, runFold, rmapFold)

--------------------------------------------------------------------------------
-- Recipe
--------------------------------------------------------------------------------

data FileRecipe a
    = ModuleRecipe
      -- ^ A recipe to create a Haskell module file.
        { moduleName :: String
        -- ^ Module name
        , generateModule :: BB.Builder -> Fold a BB.Builder }
        -- ^ Function that generate the module, given the module name.
    -- May be useful someday
    -- TestOutputRecipe
    --   -- ^ A recipe to create a test output file.
    --     String
    --     -- ^ Test name
    --     (Fold a BB.Builder)
    --     -- ^ Test output generator

-- ModuleRecipe is a tuple of the module name and a function that generates the
-- module using the module name
type ModuleRecipe a = (String, BB.Builder -> Fold a BB.Builder)

type GeneratorRecipe a = [FileRecipe a]

--------------------------------------------------------------------------------
-- Generator
--------------------------------------------------------------------------------

moduleToFileName :: String -> String
moduleToFileName = map (\x -> if x == '.' then '/' else x)

dirFromFileName :: String -> String
dirFromFileName = reverse . dropWhile (/= '/') . reverse

moduleFileEmitter :: Version -> FilePath -> FilePath -> ModuleRecipe a -> Fold a (IO ())
moduleFileEmitter version unicodeSourceFile outdir (modName, fldGen) =
    rmapFold action $ fldGen (BB.string7 modName)

    where

    pretext = mconcat
        [ "-- autogenerated from https://www.unicode.org/Public/"
        , BB.string7 (showVersion version)
        , "/ucd/"
        , BB.string7 unicodeSourceFile
        ,"\n"
        ]
    outfile = outdir </> moduleToFileName modName <.> "hs"
    outfiledir = dirFromFileName outfile
    action c = do
        createDirectoryIfMissing True outfiledir
        B.writeFile outfile (BL.toStrict (BB.toLazyByteString (pretext <> c)))

runGenerator ::
       Version
    -> FilePath
    -> FilePath
    -> (B.ByteString -> [a])
    -> FilePath
    -> GeneratorRecipe a
    -> IO ()
runGenerator version indir file transformLines outdir recipes = do
    raw <- B.readFile (indir </> file)
    sequence_ (runFold combinedFld (transformLines raw))

    where

    generatedFolds = recipes <&> \case
      ModuleRecipe     name f -> moduleFileEmitter version file outdir (name, f)
    combinedFld = distribute generatedFolds

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

apacheLicense
    :: Word   -- ^ Copyright year
    -> BB.Builder -- ^ Module name
    -> BB.Builder
apacheLicense year modName =
    unlinesBB
        [ "-- |"
        , "-- Module      : " <> modName
        , "-- Copyright   : (c) "
            <> BB.wordDec year
            <> " Composewell Technologies and Contributors"
        , "-- License     : Apache-2.0"
        , "-- Maintainer  : streamly@composewell.com"
        , "-- Stability   : experimental"
        ]

--------------------------------------------------------------------------------
-- Bitmaps
--------------------------------------------------------------------------------

genBitmap :: HasCallStack => BB.Builder -> [Int] -> BB.Builder
genBitmap funcName ordList = mconcat
    [ "{-# INLINE " <> funcName <> " #-}\n"
    , funcName, " :: Char -> Bool\n"
    , funcName, func
    , "    !(Ptr bitmap#) = ", bitmapLookup, "\n\n"
    , bitmapLookup, " :: Ptr Word8\n"
    , bitmapLookup, " = Ptr\n"
    , "    \"", bitMapToAddrLiteral bitmap "\"#\n" ]
    where
    rawBitmap = positionsToBitMap ordList
    bitmapLookup = funcName <> "Bitmap"
    (func, bitmap) = if length rawBitmap <= 0x40000
        -- Only planes 0-3
        then
            ( mconcat
                [ " = \\c -> let cp = ord c in cp >= 0x"
                , showPaddedHeXB (minimum ordList)
                , " && cp <= 0x"
                , showPaddedHeXB (maximum ordList)
                , " && lookupBit64 bitmap# cp\n"
                , "    where\n" ]
            , rawBitmap )
        -- Planes 0-3 and 14
        else
            let (planes0To3, plane14) = splitPlanes "genBitmap: cannot build" not rawBitmap
                bound0 = pred (minimum ordList)
                bound1 = length planes0To3
                bound2 = 0xE0000 + length plane14
            in ( mconcat
                    [ " c\n"
                    , if bound0 > 0
                        then mconcat
                            [ "    | cp < 0x"
                            , showPaddedHeXB bound0
                            , " = False\n" ]
                        else ""
                    , "    | cp < 0x", showPaddedHeXB bound1
                    , " = lookupBit64 bitmap# cp\n"
                    , "    | cp < 0xE0000 = False\n"
                    , "    | cp < 0x", showPaddedHeXB bound2
                    , " = lookupBit64 bitmap# (cp - 0x"
                    , showPaddedHeXB (0xE0000 - bound1)
                    , ")\n"
                    , "    | otherwise = False\n"
                    , "    where\n"
                    , "    cp = ord c\n" ]
                , planes0To3 <> plane14 )

positionsToBitMap :: [Int] -> [Bool]
positionsToBitMap = go 0

    where

    go _ [] = []
    go i xxs@(x:xs)
        | i < x = False : go (i + 1) xxs
        | otherwise = True : go (i + 1) xs

bitMapToAddrLiteral ::
  -- | Values to encode
  [Bool] ->
  -- | String to append
  BB.Builder ->
  BB.Builder
bitMapToAddrLiteral bs = chunkAddrLiteral 4 0xff encode (L.unfoldr mkChunks bs)

    where

    mkChunks :: [a] -> Maybe ([a], [a])
    mkChunks [] = Nothing
    mkChunks xs = Just $ splitAt 8 xs

    encode :: [Bool] -> BB.Builder -> BB.Builder
    encode chunk acc = BB.char7 '\\' <> BB.intDec (toByte (padTo8 chunk)) <> acc

    padTo8 :: [Bool] -> [Bool]
    padTo8 xs
        | length xs >= 8 = xs
        | otherwise = xs <> replicate (8 - length xs) False

    toByte :: [Bool] -> Int
    toByte xs = sum $ map (\i -> if xs !! i then 1 `shiftL` i else 0) [0..7]

splitPlanes :: (HasCallStack) => String -> (a -> Bool) -> [a] -> ([a], [a])
splitPlanes msg isDef xs = if all isDef planes4To13 && null planes15To16
    then (planes0To3, plane14)
    else error msg
    where
    planes0To3 = L.dropWhileEnd isDef (take 0x40000 xs)
    planes4To16 = drop 0x40000 xs
    planes4To13 = take (0xE0000 - 0x40000) planes4To16
    planes14To16 = drop (0xE0000 - 0x40000) planes4To16
    plane14 = L.dropWhileEnd isDef (take 0x10000 planes14To16)
    planes15To16 = drop 0x10000 planes14To16

genEnumBitmap
  :: forall a. (HasCallStack, Bounded a, Enum a, Eq a, Show a)
  => BB.Builder
  -- ^ Function name
  -> (a, BB.Builder)
  -- ^ Value for planes 15-16
  -> (a, BB.Builder)
  -- ^ Default value
  -> [a]
  -- ^ List of values to encode for planes 0 to 3
  -> [a]
  -- ^ List of values to encode for plane 14
  -> BB.Builder
genEnumBitmap funcName (defPUA, pPUA) (def, pDef) planes0To3 plane14 =
    mconcat
    [ "{-# INLINE ", funcName, " #-}\n"
    , funcName, " :: Char -> Int\n"
    , funcName, func
    , "    !(Ptr bitmap#) = ", bitmapLookup, "\n\n"
    , bitmapLookup, " :: Ptr Word8\n"
    , bitmapLookup, " = Ptr\n"
    , "    \"", enumMapToAddrLiteral 4 0xff bitmap "\"#"
    ]
    where
    bitmapLookup = funcName <> "Bitmap"
    planes0To3' = L.dropWhileEnd (== def) planes0To3
    check = if length planes0To3 <= 0x40000
        then ()
        else error "genEnumBitmap: Cannot build"
    (func, bitmap) = check `seq` if null plane14 && defPUA == def
        -- Only planes 0-3
        then
            ( mconcat
                [ " = \\c -> let cp = ord c in if cp >= 0x"
                , showPaddedHeXB (length planes0To3')
                , " then "
                , pDef
                , " else lookupIntN bitmap# cp\n"
                , "    where\n" ]
            , planes0To3' )
        -- All the planes
        else
            let plane14' = L.dropWhileEnd (== def) plane14
                bound1 = length planes0To3'
                bound2 = 0xE0000 + length plane14'
            in ( mconcat
                    [ " c\n"
                    , "    -- Planes 0-3\n"
                    , "    | cp < 0x", showPaddedHeXB bound1
                                     , " = lookupIntN bitmap# cp\n"
                    , "    -- Planes 4-13: ", showB def, "\n"
                    , "    | cp < 0xE0000 = " <> pDef, "\n"
                    , "    -- Plane 14\n"
                    , "    | cp < 0x", showPaddedHeXB bound2
                                     , " = lookupIntN bitmap# (cp - 0x"
                                     , showPaddedHeXB (0xE0000 - bound1)
                                     , ")\n"
                    , if defPUA == def
                        then ""
                        else mconcat
                            [ "    -- Plane 14: ", showB def, "\n"
                            , "    | cp < 0xF0000 = ", pDef, "\n"
                            , "    -- Plane 15: ", showB defPUA, "\n"
                            , "    | cp < 0xFFFFE = ", pPUA, "\n"
                            , "    -- Plane 15: ", showB def, "\n"
                            , "    | cp < 0x100000 = ", pDef, "\n"
                            , "    -- Plane 16: ", showB defPUA, "\n"
                            , "    | cp < 0x10FFFE = ", pPUA, "\n" ]
                    , "    -- Default: ", showB def, "\n"
                    , "    | otherwise = " <> pDef, "\n"
                    , "    where\n"
                    , "    cp = ord c\n" ]
                , planes0To3' <> plane14' )

{-| Encode a list of values as a byte map, using their 'Enum' instance.

__Note:__ 'Enum' instance must respect the following:

* @fromEnum minBound >= 0x00@
* @fromEnum maxBound <= 0xff@
-}
enumMapToAddrLiteral
  :: forall a. (Bounded a, Enum a, Show a)
  => Word8
  -- ^ Indentation
  -> Int
  -- ^ Chunk size
  -> [a]
  -- ^ Values to encode
  -> BB.Builder
  -- ^ String to append
  -> BB.Builder
enumMapToAddrLiteral indentation chunkSize =
    chunkAddrLiteral indentation chunkSize addWord

    where

    addWord :: a -> BB.Builder -> BB.Builder
    addWord x acc = BB.char7 '\\' <> BB.word8Dec (toWord8 x) <> acc

    toWord8 :: a -> Word8
    toWord8 a = let w = fromEnum a in if 0 <= w && w <= 0xff
        then fromIntegral w
        else error $ "Cannot convert to Word8: " <> show a

chunkAddrLiteral
  :: forall a. Word8
  -- ^ Indentation
  -> Int
  -- ^ Chunk size
  -> (a -> BB.Builder -> BB.Builder)
  -- ^ Function to convert to 'Word8' and prepend to the accumulator
  -> [a]
  -- ^ Values to encode
  -> BB.Builder
  -- ^ String to append
  -> BB.Builder
chunkAddrLiteral indentation chunkSize addWord xs cs
    = fst
    . foldr go (cs, NoIndent)
    $ chunksOf chunkSize xs

    where

    indent = indent' indentation . (BB.char7 '\\' <>)
    indent' = \case
        0 -> (BB.shortByteString "\\\n" <>)
        i -> indent' (pred i) . (BB.char7 ' ' <>)

    go :: [a] -> (BB.Builder, Indent) -> (BB.Builder, Indent)
    go as (acc, seps) = (foldr addWord (f acc) as, Indent)
        where
        f = case seps of
            NoIndent -> id
            Indent -> indent

data Indent = NoIndent | Indent

chunksOf :: Int -> [a] -> [[a]]
chunksOf i = go
    where
    go = \case
        [] -> []
        as -> b : go as'
            where (b, as') = splitAt i as

-- Encode Word32 to [Word8] little endian
word32ToWord8s :: Word32 -> [Word8]
word32ToWord8s n = (\k -> fromIntegral ((n `shiftR` k) .&. 0xff)) <$> [0,8..24]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

unlinesBB :: [BB.Builder] -> BB.Builder
unlinesBB = (<> "\n") . mconcat . L.intersperse "\n"

unwordsBB :: [BB.Builder] -> BB.Builder
unwordsBB = mconcat . L.intersperse " "
