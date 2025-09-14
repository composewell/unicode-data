-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.UnicodeData.DerivedNames (recipe) where

import Data.Bits (Bits(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
import Data.Char (ord)
import Data.Word (Word8)
import qualified Unicode.CharacterDatabase.Parser.Extracted.DerivedName as N

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense, enumMapToAddrLiteral8, chunkAddrLiteral)
import UCD2Haskell.Common (Fold (..), showPaddedHeXB, showHexCodepoint, showPaddedHeX, showHexCodepointB, showHexCodepointBS)

recipe :: FileRecipe N.Entry
recipe = ModuleRecipe
    "Unicode.Internal.Char.UnicodeData.DerivedName"
    genNamesModule

data Acc = Acc
    { expected :: !Char
    , names :: ![BS.ShortByteString]
    , offsets :: ![[Int]]
    , offset :: !Int
    , maxPlanes03 :: !Char
    , maxPlane14 :: !Char
    }

genNamesModule :: BB.Builder -> Fold N.Entry BB.Builder
genNamesModule moduleName = Fold step initial done
    where

    initial = Acc '\0' mempty mempty 0 '\0' '\0'

    step acc = \case
        N.SingleChar{..} -> step' acc char name
        N.CharRange{..} -> if prefix `elem` rangePrefixes
            then foldl'
                (\a c -> step' a c (mkName prefix c))
                acc
                [start..end]
            else error . mconcat $
                [ "Unexpected name range: "
                , show prefix
                , ". Please update the generator and the "
                , "Unicode.Char.General.Names* modules" ]

    mkName prefix c = prefix <> showHexCodepointBS c

    step' Acc{..} char name = if expected < char
            then if expected < '\xE0000' && char >= '\xE0000'
                then step'
                    Acc { expected = '\xE0000'
                        , names
                        , offsets
                        , offset
                        , maxPlanes03
                        , maxPlane14 }
                    char name
                else step'
                    Acc { expected = succ expected
                        , names
                        , offsets = encodeOffset 0 0 : offsets
                        , offset
                        , maxPlanes03
                        , maxPlane14 }
                    char name
            else
                let !(name', len, len', compressed) = encodeName name
                in if (char < '\x40000' || char >= '\xE0000') &&
                    offset <= 0xffffff && (len < hangul || compressed)
                    then Acc
                        { expected = succ expected
                        , names = name' : names
                        , offsets = encodeOffset offset len : offsets
                        , offset = offset + len'
                        , maxPlanes03 = if char < '\x40000'
                            then max maxPlanes03 char
                            else maxPlanes03
                        , maxPlane14 = max maxPlane14 char }
                    else error (mconcat
                        [ "genNamesModule: Cannot encode '\\x"
                        , showHexCodepoint char
                        , "' ", show name
                        , " (offset: 0x"
                        , showPaddedHeX offset
                        , ", length: 0x"
                        , showPaddedHeX len
                        , ")" ])

    cjkCompat = 0xf0
    cjkUnified = 0xf1
    tangut = 0xf2
    egyptianHieroglyph = 0xf3
    khitan = 0xf4
    nushu = 0xf5
    hangul = 0x80

    rangePrefixes =
        [ "CJK COMPATIBILITY IDEOGRAPH-"
        , "CJK UNIFIED IDEOGRAPH-"
        , "TANGUT IDEOGRAPH-"
        , "EGYPTIAN HIEROGLYPH-"
        , "KHITAN SMALL SCRIPT CHARACTER-"
        , "NUSHU CHARACTER-" ]

    encodeName name
        | BS.take 28 name == "CJK COMPATIBILITY IDEOGRAPH-"   = ("", cjkCompat, 0, True)
        | BS.take 22 name == "CJK UNIFIED IDEOGRAPH-"         = ("", cjkUnified, 0, True)
        | BS.take 17 name == "TANGUT IDEOGRAPH-"              = ("", tangut, 0, True)
        | BS.take 20 name == "EGYPTIAN HIEROGLYPH-"           = ("", egyptianHieroglyph, 0, True)
        | BS.take 30 name == "KHITAN SMALL SCRIPT CHARACTER-" = ("", khitan, 0, True)
        | BS.take 16 name == "NUSHU CHARACTER-"               = ("", nushu, 0, True)
        | BS.take 16 name == "HANGUL SYLLABLE "               =
            let !name' = BS.drop 16 name; !len = BS.length name'
            in if len <= 12
                then (name', hangul + len, len, True)
                else error ("genNamesModule: cannot encode Hangul: " <> show len)
        | otherwise = let !len = BS.length name in (name, len, len, False)

    encodeOffset offset len = encode32LE offset' mempty
        where !offset' = len .|. (offset `shiftL` 8)
    encode32LE v acc
        = (v             .&. 0xff)
        : (v `shiftR` 8  .&. 0xff)
        : (v `shiftR` 16 .&. 0xff)
        :  v `shiftR` 24
        : acc

    done Acc{..} = unlinesBB
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE PatternSynonyms #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "    ( name"
        , "    , pattern NoName"
        , "    , pattern CjkCompatibilityIdeograph"
        , "    , pattern CjkUnifiedIdeograph"
        , "    , pattern TangutIdeograph"
        , "    , pattern EgyptianHieroglyph"
        , "    , pattern KhitanSmallScript"
        , "    , pattern NushuCharacter"
        , "    , pattern HangulSyllable"
        , "    ) where"
        , ""
        , "import Data.Int (Int32)"
        , "import Foreign.C (CChar)"
        , "import GHC.Exts"
        , "    ( Addr#, Char#, Int#, Ptr(..),"
        , "      ord#, (-#), (<#),"
        , "      uncheckedIShiftRL#, andI#,"
        , "      plusAddr#, isTrue# )"
        , "import Unicode.Internal.Bits.Names (lookupInt32#)"
        , ""
        , "-- | No name. Used to test length returned by 'name'."
        , "--"
        , "-- @since 0.3.0"
        , "pattern NoName :: Int#"
        , "pattern NoName = 0#"
        , ""
        , "-- | CJK compatibility ideograph. Used to test the length returned by 'name'."
        , "--"
        , "-- @since 0.3.0"
        , "pattern CjkCompatibilityIdeograph :: Int#"
        , "pattern CjkCompatibilityIdeograph = 0x" <> intHex cjkCompat <> "#"
        , ""
        , "-- | CJK unified ideograph. Used to test the length returned by 'name'."
        , "--"
        , "-- @since 0.3.0"
        , "pattern CjkUnifiedIdeograph :: Int#"
        , "pattern CjkUnifiedIdeograph = 0x" <> intHex cjkUnified <> "#"
        , ""
        , "-- | Tangut ideograph. Used to test the length returned by 'name'."
        , "--"
        , "-- @since 0.3.0"
        , "pattern TangutIdeograph :: Int#"
        , "pattern TangutIdeograph = 0x" <> intHex tangut <> "#"
        , ""
        , "-- | Egyptian Hieroglyph. Used to test the length returned by 'name'."
        , "--"
        , "-- @since 0.5.0"
        , "pattern EgyptianHieroglyph :: Int#"
        , "pattern EgyptianHieroglyph = 0x" <> intHex egyptianHieroglyph <> "#"
        , ""
        , "-- | Khitan Small Script. Used to test the length returned by 'name'."
        , "--"
        , "-- @since 0.5.0"
        , "pattern KhitanSmallScript :: Int#"
        , "pattern KhitanSmallScript = 0x" <> intHex khitan <> "#"
        , ""
        , "-- | Nushu Character. Used to test the length returned by 'name'."
        , "--"
        , "-- @since 0.5.0"
        , "pattern NushuCharacter :: Int#"
        , "pattern NushuCharacter = 0x" <> intHex nushu <> "#"
        , ""
        , "-- | Hangul syllable. Used to test the length returned by 'name'."
        , "--"
        , "-- @since 0.3.0"
        , "pattern HangulSyllable :: Int#"
        , "pattern HangulSyllable = 0x" <> intHex hangul <> "#"
        , ""
        , "-- | Name of a character, if defined."
        , "--"
        , "-- The return value represents: (ASCII string, string length or special value)."
        , "--"
        , "-- Some characters require specific processing:"
        , "--"
        , "-- * If length = @'CjkCompatibilityIdeograph'@,"
        , "--   then the name is generated from the pattern “CJK COMPATIBILITY IDEOGRAPH-*”,"
        , "--   where * is the hexadecimal codepoint."
        , "-- * If length = @'CjkUnifiedIdeograph'@,"
        , "--   then the name is generated from the pattern “CJK UNIFIED IDEOGRAPH-*”,"
        , "--   where * is the hexadecimal codepoint."
        , "-- * If length = @'TangutIdeograph'@,"
        , "--   then the name is generated from the pattern “TANGUT IDEOGRAPH-*”,"
        , "--   where * is the hexadecimal codepoint."
        , "-- * If length = @'EgyptianHieroglyph'@,"
        , "--   then the name is generated from the pattern “EGYPTIAN HIEROGLYPH-*”,"
        , "--   where * is the hexadecimal codepoint."
        , "-- * If length = @'KhitanSmallScript'@,"
        , "--   then the name is generated from the pattern “KHITAN SMALL SCRIPT CHARACTER-*”,"
        , "--   where * is the hexadecimal codepoint."
        , "-- * If length = @'NushuCharacter'@,"
        , "--   then the name is generated from the pattern “NUSHU CHARACTER-*”,"
        , "--   where * is the hexadecimal codepoint."
        , "-- * If length ≥ @'HangulSyllable'@,"
        , "--   then the name is generated by prepending “HANGUL SYLLABLE ”"
        , "--   to the returned string."
        , "--"
        , "-- See an example of such implementation using 'String's in 'Unicode.Char.General.Names.name'."
        , "--"
        , "-- @since 0.1.0"
        , "{-# INLINE name #-}"
        , "name :: Char# -> (# Addr#, Int# #)"
        , "name c#"
        , "    | isTrue# (cp# <# 0x"
            <> showHexCodepointB (succ maxPlanes03)
            <> "#) = getName cp#"
        , "    | isTrue# (cp# <# 0xE0000#) = (# \"\\0\"#, 0# #)"
        , "    | isTrue# (cp# <# 0x"
            <> showHexCodepointB (succ maxPlane14)
            <> "#) = getName (cp# -# 0x"
            <> showPaddedHeXB (0xE0000 - ord (succ maxPlanes03))
            <> "#)"
        , "    | otherwise = (# \"\\0\"#, 0# #)"
        , ""
        , "    where"
        , ""
        , "    -- [NOTE] Encoding"
        , "    -- • The names are ASCII. Each name is encoded as a raw bytes literal."
        , "    -- • The names are concatenated in names#."
        , "    --   There are exceptions (see function’s doc)."
        , "    -- • The name of a character, if defined, is referenced by an offset in names#."
        , "    -- • The offsets are stored in offsets#. A character entry is composed of:"
        , "    --   • a LE Word24 for the offset;"
        , "    --   • a Word8 for the length of the name or a special value."
        , ""
        , "    !cp# = ord# c#"
        , ""
        , "    {-# INLINE getName #-}"
        , "    getName k# ="
        , "        let !entry# = lookupInt32# offsets# k#"
        , "            !offset# = entry# `uncheckedIShiftRL#` 8#"
        , "            !name# = names# `plusAddr#` offset#"
        , "            !len# = entry# `andI#` 0xff#"
        , "        in (# name#, len# #)"
        , ""
        , "    !(Ptr names#) = namesBitmap"
        , "    !(Ptr offsets#) = offsetsBitmap"
        , ""
        , "namesBitmap :: Ptr CChar"
        , "namesBitmap = Ptr"
        , "    \""
            <> chunkAddrLiteral 4 0xff showsB (shortByteStringsToString names) "\"#"
        , ""
        , "offsetsBitmap :: Ptr Int32"
        , "offsetsBitmap = Ptr"
        , "    \""
            <> enumMapToAddrLiteral8 4 0xff (mconcat (reverse offsets)) "\"#"
        ]
        where
        showsB :: Word8 -> BB.Builder -> BB.Builder
        showsB = \case
            0 -> (BB.string7 "\\0" <>)
            c -> (BB.word8 c <>) -- Note: names are ASCII
        intHex = BB.wordHex . fromIntegral
        shortByteStringsToString
            = BL.unpack
            . BB.toLazyByteString
            . foldMap BB.shortByteString
            . reverse
