-- autogenerated from https://www.unicode.org/Public/15.0.0/ucd/NameAliases.txt
-- |
-- Module      : Unicode.Internal.Char.UnicodeData.NameAliases
-- Copyright   : (c) 2022 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental

{-# OPTIONS_HADDOCK hide #-}

module Unicode.Internal.Char.UnicodeData.NameAliases
(NameAliasType(..), nameAliases, nameAliasesByType, nameAliasesWithTypes)
where

import Data.Ix (Ix)
import Data.Maybe (fromMaybe)
import Foreign.C.String (CString)
import GHC.Exts (Ptr(..))

-- | Type of name alias. See Unicode Standard 14.0.0, section 4.8.
--
-- @since 0.1.0
data NameAliasType
    = Correction
    -- ^ Corrections for serious problems in the character names.
    | Control
    -- ^ ISO&#xa0;6429 names for @C0@ and @C1@ control functions, and other
    --   commonly occurring names for control codes.
    | Alternate
    -- ^ A few widely used alternate names for format characters.
    | Figment
    -- ^ Several documented labels for @C1@ control code points which
    --   were never actually approved in any standard.
    | Abbreviation
    -- ^ Commonly occurring abbreviations (or acronyms) for control codes,
    --   format characters, spaces, and variation selectors.
    deriving (Enum, Bounded, Eq, Ord, Ix, Show)

-- | All name aliases of a character.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliasesWithTypes' for the detailed list by alias type.
--
-- @since 0.1.0
{-# INLINE nameAliases #-}
nameAliases :: Char -> [CString]
nameAliases = mconcat . fmap snd . nameAliasesWithTypes

-- | Name aliases of a character for a specific name alias type.
--
-- @since 0.1.0
{-# INLINE nameAliasesByType #-}
nameAliasesByType :: NameAliasType -> Char -> [CString]
nameAliasesByType t = fromMaybe mempty . lookup t . nameAliasesWithTypes

-- | Detailed character names aliases.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliases' if the alias type is not required.
--
-- @since 0.1.0
nameAliasesWithTypes :: Char -> [(NameAliasType, [CString])]
nameAliasesWithTypes = \case
  '\x0000' -> [(Control,[Ptr "NULL\0"#]),(Abbreviation,[Ptr "NUL\0"#])]
  '\x0001' -> [(Control,[Ptr "START OF HEADING\0"#]),(Abbreviation,[Ptr "SOH\0"#])]
  '\x0002' -> [(Control,[Ptr "START OF TEXT\0"#]),(Abbreviation,[Ptr "STX\0"#])]
  '\x0003' -> [(Control,[Ptr "END OF TEXT\0"#]),(Abbreviation,[Ptr "ETX\0"#])]
  '\x0004' -> [(Control,[Ptr "END OF TRANSMISSION\0"#]),(Abbreviation,[Ptr "EOT\0"#])]
  '\x0005' -> [(Control,[Ptr "ENQUIRY\0"#]),(Abbreviation,[Ptr "ENQ\0"#])]
  '\x0006' -> [(Control,[Ptr "ACKNOWLEDGE\0"#]),(Abbreviation,[Ptr "ACK\0"#])]
  '\x0007' -> [(Control,[Ptr "ALERT\0"#]),(Abbreviation,[Ptr "BEL\0"#])]
  '\x0008' -> [(Control,[Ptr "BACKSPACE\0"#]),(Abbreviation,[Ptr "BS\0"#])]
  '\x0009' -> [(Control,[Ptr "CHARACTER TABULATION\0"#,Ptr "HORIZONTAL TABULATION\0"#]),(Abbreviation,[Ptr "HT\0"#,Ptr "TAB\0"#])]
  '\x000a' -> [(Control,[Ptr "LINE FEED\0"#,Ptr "NEW LINE\0"#,Ptr "END OF LINE\0"#]),(Abbreviation,[Ptr "LF\0"#,Ptr "NL\0"#,Ptr "EOL\0"#])]
  '\x000b' -> [(Control,[Ptr "LINE TABULATION\0"#,Ptr "VERTICAL TABULATION\0"#]),(Abbreviation,[Ptr "VT\0"#])]
  '\x000c' -> [(Control,[Ptr "FORM FEED\0"#]),(Abbreviation,[Ptr "FF\0"#])]
  '\x000d' -> [(Control,[Ptr "CARRIAGE RETURN\0"#]),(Abbreviation,[Ptr "CR\0"#])]
  '\x000e' -> [(Control,[Ptr "SHIFT OUT\0"#,Ptr "LOCKING-SHIFT ONE\0"#]),(Abbreviation,[Ptr "SO\0"#])]
  '\x000f' -> [(Control,[Ptr "SHIFT IN\0"#,Ptr "LOCKING-SHIFT ZERO\0"#]),(Abbreviation,[Ptr "SI\0"#])]
  '\x0010' -> [(Control,[Ptr "DATA LINK ESCAPE\0"#]),(Abbreviation,[Ptr "DLE\0"#])]
  '\x0011' -> [(Control,[Ptr "DEVICE CONTROL ONE\0"#]),(Abbreviation,[Ptr "DC1\0"#])]
  '\x0012' -> [(Control,[Ptr "DEVICE CONTROL TWO\0"#]),(Abbreviation,[Ptr "DC2\0"#])]
  '\x0013' -> [(Control,[Ptr "DEVICE CONTROL THREE\0"#]),(Abbreviation,[Ptr "DC3\0"#])]
  '\x0014' -> [(Control,[Ptr "DEVICE CONTROL FOUR\0"#]),(Abbreviation,[Ptr "DC4\0"#])]
  '\x0015' -> [(Control,[Ptr "NEGATIVE ACKNOWLEDGE\0"#]),(Abbreviation,[Ptr "NAK\0"#])]
  '\x0016' -> [(Control,[Ptr "SYNCHRONOUS IDLE\0"#]),(Abbreviation,[Ptr "SYN\0"#])]
  '\x0017' -> [(Control,[Ptr "END OF TRANSMISSION BLOCK\0"#]),(Abbreviation,[Ptr "ETB\0"#])]
  '\x0018' -> [(Control,[Ptr "CANCEL\0"#]),(Abbreviation,[Ptr "CAN\0"#])]
  '\x0019' -> [(Control,[Ptr "END OF MEDIUM\0"#]),(Abbreviation,[Ptr "EOM\0"#,Ptr "EM\0"#])]
  '\x001a' -> [(Control,[Ptr "SUBSTITUTE\0"#]),(Abbreviation,[Ptr "SUB\0"#])]
  '\x001b' -> [(Control,[Ptr "ESCAPE\0"#]),(Abbreviation,[Ptr "ESC\0"#])]
  '\x001c' -> [(Control,[Ptr "INFORMATION SEPARATOR FOUR\0"#,Ptr "FILE SEPARATOR\0"#]),(Abbreviation,[Ptr "FS\0"#])]
  '\x001d' -> [(Control,[Ptr "INFORMATION SEPARATOR THREE\0"#,Ptr "GROUP SEPARATOR\0"#]),(Abbreviation,[Ptr "GS\0"#])]
  '\x001e' -> [(Control,[Ptr "INFORMATION SEPARATOR TWO\0"#,Ptr "RECORD SEPARATOR\0"#]),(Abbreviation,[Ptr "RS\0"#])]
  '\x001f' -> [(Control,[Ptr "INFORMATION SEPARATOR ONE\0"#,Ptr "UNIT SEPARATOR\0"#]),(Abbreviation,[Ptr "US\0"#])]
  '\x0020' -> [(Abbreviation,[Ptr "SP\0"#])]
  '\x007f' -> [(Control,[Ptr "DELETE\0"#]),(Abbreviation,[Ptr "DEL\0"#])]
  '\x0080' -> [(Figment,[Ptr "PADDING CHARACTER\0"#]),(Abbreviation,[Ptr "PAD\0"#])]
  '\x0081' -> [(Figment,[Ptr "HIGH OCTET PRESET\0"#]),(Abbreviation,[Ptr "HOP\0"#])]
  '\x0082' -> [(Control,[Ptr "BREAK PERMITTED HERE\0"#]),(Abbreviation,[Ptr "BPH\0"#])]
  '\x0083' -> [(Control,[Ptr "NO BREAK HERE\0"#]),(Abbreviation,[Ptr "NBH\0"#])]
  '\x0084' -> [(Control,[Ptr "INDEX\0"#]),(Abbreviation,[Ptr "IND\0"#])]
  '\x0085' -> [(Control,[Ptr "NEXT LINE\0"#]),(Abbreviation,[Ptr "NEL\0"#])]
  '\x0086' -> [(Control,[Ptr "START OF SELECTED AREA\0"#]),(Abbreviation,[Ptr "SSA\0"#])]
  '\x0087' -> [(Control,[Ptr "END OF SELECTED AREA\0"#]),(Abbreviation,[Ptr "ESA\0"#])]
  '\x0088' -> [(Control,[Ptr "CHARACTER TABULATION SET\0"#,Ptr "HORIZONTAL TABULATION SET\0"#]),(Abbreviation,[Ptr "HTS\0"#])]
  '\x0089' -> [(Control,[Ptr "CHARACTER TABULATION WITH JUSTIFICATION\0"#,Ptr "HORIZONTAL TABULATION WITH JUSTIFICATION\0"#]),(Abbreviation,[Ptr "HTJ\0"#])]
  '\x008a' -> [(Control,[Ptr "LINE TABULATION SET\0"#,Ptr "VERTICAL TABULATION SET\0"#]),(Abbreviation,[Ptr "VTS\0"#])]
  '\x008b' -> [(Control,[Ptr "PARTIAL LINE FORWARD\0"#,Ptr "PARTIAL LINE DOWN\0"#]),(Abbreviation,[Ptr "PLD\0"#])]
  '\x008c' -> [(Control,[Ptr "PARTIAL LINE BACKWARD\0"#,Ptr "PARTIAL LINE UP\0"#]),(Abbreviation,[Ptr "PLU\0"#])]
  '\x008d' -> [(Control,[Ptr "REVERSE LINE FEED\0"#,Ptr "REVERSE INDEX\0"#]),(Abbreviation,[Ptr "RI\0"#])]
  '\x008e' -> [(Control,[Ptr "SINGLE SHIFT TWO\0"#,Ptr "SINGLE-SHIFT-2\0"#]),(Abbreviation,[Ptr "SS2\0"#])]
  '\x008f' -> [(Control,[Ptr "SINGLE SHIFT THREE\0"#,Ptr "SINGLE-SHIFT-3\0"#]),(Abbreviation,[Ptr "SS3\0"#])]
  '\x0090' -> [(Control,[Ptr "DEVICE CONTROL STRING\0"#]),(Abbreviation,[Ptr "DCS\0"#])]
  '\x0091' -> [(Control,[Ptr "PRIVATE USE ONE\0"#,Ptr "PRIVATE USE-1\0"#]),(Abbreviation,[Ptr "PU1\0"#])]
  '\x0092' -> [(Control,[Ptr "PRIVATE USE TWO\0"#,Ptr "PRIVATE USE-2\0"#]),(Abbreviation,[Ptr "PU2\0"#])]
  '\x0093' -> [(Control,[Ptr "SET TRANSMIT STATE\0"#]),(Abbreviation,[Ptr "STS\0"#])]
  '\x0094' -> [(Control,[Ptr "CANCEL CHARACTER\0"#]),(Abbreviation,[Ptr "CCH\0"#])]
  '\x0095' -> [(Control,[Ptr "MESSAGE WAITING\0"#]),(Abbreviation,[Ptr "MW\0"#])]
  '\x0096' -> [(Control,[Ptr "START OF GUARDED AREA\0"#,Ptr "START OF PROTECTED AREA\0"#]),(Abbreviation,[Ptr "SPA\0"#])]
  '\x0097' -> [(Control,[Ptr "END OF GUARDED AREA\0"#,Ptr "END OF PROTECTED AREA\0"#]),(Abbreviation,[Ptr "EPA\0"#])]
  '\x0098' -> [(Control,[Ptr "START OF STRING\0"#]),(Abbreviation,[Ptr "SOS\0"#])]
  '\x0099' -> [(Figment,[Ptr "SINGLE GRAPHIC CHARACTER INTRODUCER\0"#]),(Abbreviation,[Ptr "SGC\0"#])]
  '\x009a' -> [(Control,[Ptr "SINGLE CHARACTER INTRODUCER\0"#]),(Abbreviation,[Ptr "SCI\0"#])]
  '\x009b' -> [(Control,[Ptr "CONTROL SEQUENCE INTRODUCER\0"#]),(Abbreviation,[Ptr "CSI\0"#])]
  '\x009c' -> [(Control,[Ptr "STRING TERMINATOR\0"#]),(Abbreviation,[Ptr "ST\0"#])]
  '\x009d' -> [(Control,[Ptr "OPERATING SYSTEM COMMAND\0"#]),(Abbreviation,[Ptr "OSC\0"#])]
  '\x009e' -> [(Control,[Ptr "PRIVACY MESSAGE\0"#]),(Abbreviation,[Ptr "PM\0"#])]
  '\x009f' -> [(Control,[Ptr "APPLICATION PROGRAM COMMAND\0"#]),(Abbreviation,[Ptr "APC\0"#])]
  '\x00a0' -> [(Abbreviation,[Ptr "NBSP\0"#])]
  '\x00ad' -> [(Abbreviation,[Ptr "SHY\0"#])]
  '\x01a2' -> [(Correction,[Ptr "LATIN CAPITAL LETTER GHA\0"#])]
  '\x01a3' -> [(Correction,[Ptr "LATIN SMALL LETTER GHA\0"#])]
  '\x034f' -> [(Abbreviation,[Ptr "CGJ\0"#])]
  '\x0616' -> [(Correction,[Ptr "ARABIC SMALL HIGH LIGATURE ALEF WITH YEH BARREE\0"#])]
  '\x061c' -> [(Abbreviation,[Ptr "ALM\0"#])]
  '\x0709' -> [(Correction,[Ptr "SYRIAC SUBLINEAR COLON SKEWED LEFT\0"#])]
  '\x0cde' -> [(Correction,[Ptr "KANNADA LETTER LLLA\0"#])]
  '\x0e9d' -> [(Correction,[Ptr "LAO LETTER FO FON\0"#])]
  '\x0e9f' -> [(Correction,[Ptr "LAO LETTER FO FAY\0"#])]
  '\x0ea3' -> [(Correction,[Ptr "LAO LETTER RO\0"#])]
  '\x0ea5' -> [(Correction,[Ptr "LAO LETTER LO\0"#])]
  '\x0fd0' -> [(Correction,[Ptr "TIBETAN MARK BKA- SHOG GI MGO RGYAN\0"#])]
  '\x11ec' -> [(Correction,[Ptr "HANGUL JONGSEONG YESIEUNG-KIYEOK\0"#])]
  '\x11ed' -> [(Correction,[Ptr "HANGUL JONGSEONG YESIEUNG-SSANGKIYEOK\0"#])]
  '\x11ee' -> [(Correction,[Ptr "HANGUL JONGSEONG SSANGYESIEUNG\0"#])]
  '\x11ef' -> [(Correction,[Ptr "HANGUL JONGSEONG YESIEUNG-KHIEUKH\0"#])]
  '\x180b' -> [(Abbreviation,[Ptr "FVS1\0"#])]
  '\x180c' -> [(Abbreviation,[Ptr "FVS2\0"#])]
  '\x180d' -> [(Abbreviation,[Ptr "FVS3\0"#])]
  '\x180e' -> [(Abbreviation,[Ptr "MVS\0"#])]
  '\x180f' -> [(Abbreviation,[Ptr "FVS4\0"#])]
  '\x1bbd' -> [(Correction,[Ptr "SUNDANESE LETTER ARCHAIC I\0"#])]
  '\x200b' -> [(Abbreviation,[Ptr "ZWSP\0"#])]
  '\x200c' -> [(Abbreviation,[Ptr "ZWNJ\0"#])]
  '\x200d' -> [(Abbreviation,[Ptr "ZWJ\0"#])]
  '\x200e' -> [(Abbreviation,[Ptr "LRM\0"#])]
  '\x200f' -> [(Abbreviation,[Ptr "RLM\0"#])]
  '\x202a' -> [(Abbreviation,[Ptr "LRE\0"#])]
  '\x202b' -> [(Abbreviation,[Ptr "RLE\0"#])]
  '\x202c' -> [(Abbreviation,[Ptr "PDF\0"#])]
  '\x202d' -> [(Abbreviation,[Ptr "LRO\0"#])]
  '\x202e' -> [(Abbreviation,[Ptr "RLO\0"#])]
  '\x202f' -> [(Abbreviation,[Ptr "NNBSP\0"#])]
  '\x205f' -> [(Abbreviation,[Ptr "MMSP\0"#])]
  '\x2060' -> [(Abbreviation,[Ptr "WJ\0"#])]
  '\x2066' -> [(Abbreviation,[Ptr "LRI\0"#])]
  '\x2067' -> [(Abbreviation,[Ptr "RLI\0"#])]
  '\x2068' -> [(Abbreviation,[Ptr "FSI\0"#])]
  '\x2069' -> [(Abbreviation,[Ptr "PDI\0"#])]
  '\x2118' -> [(Correction,[Ptr "WEIERSTRASS ELLIPTIC FUNCTION\0"#])]
  '\x2448' -> [(Correction,[Ptr "MICR ON US SYMBOL\0"#])]
  '\x2449' -> [(Correction,[Ptr "MICR DASH SYMBOL\0"#])]
  '\x2b7a' -> [(Correction,[Ptr "LEFTWARDS TRIANGLE-HEADED ARROW WITH DOUBLE VERTICAL STROKE\0"#])]
  '\x2b7c' -> [(Correction,[Ptr "RIGHTWARDS TRIANGLE-HEADED ARROW WITH DOUBLE VERTICAL STROKE\0"#])]
  '\xa015' -> [(Correction,[Ptr "YI SYLLABLE ITERATION MARK\0"#])]
  '\xaa6e' -> [(Correction,[Ptr "MYANMAR LETTER KHAMTI LLA\0"#])]
  '\xfe00' -> [(Abbreviation,[Ptr "VS1\0"#])]
  '\xfe01' -> [(Abbreviation,[Ptr "VS2\0"#])]
  '\xfe02' -> [(Abbreviation,[Ptr "VS3\0"#])]
  '\xfe03' -> [(Abbreviation,[Ptr "VS4\0"#])]
  '\xfe04' -> [(Abbreviation,[Ptr "VS5\0"#])]
  '\xfe05' -> [(Abbreviation,[Ptr "VS6\0"#])]
  '\xfe06' -> [(Abbreviation,[Ptr "VS7\0"#])]
  '\xfe07' -> [(Abbreviation,[Ptr "VS8\0"#])]
  '\xfe08' -> [(Abbreviation,[Ptr "VS9\0"#])]
  '\xfe09' -> [(Abbreviation,[Ptr "VS10\0"#])]
  '\xfe0a' -> [(Abbreviation,[Ptr "VS11\0"#])]
  '\xfe0b' -> [(Abbreviation,[Ptr "VS12\0"#])]
  '\xfe0c' -> [(Abbreviation,[Ptr "VS13\0"#])]
  '\xfe0d' -> [(Abbreviation,[Ptr "VS14\0"#])]
  '\xfe0e' -> [(Abbreviation,[Ptr "VS15\0"#])]
  '\xfe0f' -> [(Abbreviation,[Ptr "VS16\0"#])]
  '\xfe18' -> [(Correction,[Ptr "PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET\0"#])]
  '\xfeff' -> [(Alternate,[Ptr "BYTE ORDER MARK\0"#]),(Abbreviation,[Ptr "BOM\0"#,Ptr "ZWNBSP\0"#])]
  '\x122d4' -> [(Correction,[Ptr "CUNEIFORM SIGN NU11 TENU\0"#])]
  '\x122d5' -> [(Correction,[Ptr "CUNEIFORM SIGN NU11 OVER NU11 BUR OVER BUR\0"#])]
  '\x16e56' -> [(Correction,[Ptr "MEDEFAIDRIN CAPITAL LETTER H\0"#])]
  '\x16e57' -> [(Correction,[Ptr "MEDEFAIDRIN CAPITAL LETTER NG\0"#])]
  '\x16e76' -> [(Correction,[Ptr "MEDEFAIDRIN SMALL LETTER H\0"#])]
  '\x16e77' -> [(Correction,[Ptr "MEDEFAIDRIN SMALL LETTER NG\0"#])]
  '\x1b001' -> [(Correction,[Ptr "HENTAIGANA LETTER E-1\0"#])]
  '\x1d0c5' -> [(Correction,[Ptr "BYZANTINE MUSICAL SYMBOL FTHORA SKLIRON CHROMA VASIS\0"#])]
  '\xe0100' -> [(Abbreviation,[Ptr "VS17\0"#])]
  '\xe0101' -> [(Abbreviation,[Ptr "VS18\0"#])]
  '\xe0102' -> [(Abbreviation,[Ptr "VS19\0"#])]
  '\xe0103' -> [(Abbreviation,[Ptr "VS20\0"#])]
  '\xe0104' -> [(Abbreviation,[Ptr "VS21\0"#])]
  '\xe0105' -> [(Abbreviation,[Ptr "VS22\0"#])]
  '\xe0106' -> [(Abbreviation,[Ptr "VS23\0"#])]
  '\xe0107' -> [(Abbreviation,[Ptr "VS24\0"#])]
  '\xe0108' -> [(Abbreviation,[Ptr "VS25\0"#])]
  '\xe0109' -> [(Abbreviation,[Ptr "VS26\0"#])]
  '\xe010a' -> [(Abbreviation,[Ptr "VS27\0"#])]
  '\xe010b' -> [(Abbreviation,[Ptr "VS28\0"#])]
  '\xe010c' -> [(Abbreviation,[Ptr "VS29\0"#])]
  '\xe010d' -> [(Abbreviation,[Ptr "VS30\0"#])]
  '\xe010e' -> [(Abbreviation,[Ptr "VS31\0"#])]
  '\xe010f' -> [(Abbreviation,[Ptr "VS32\0"#])]
  '\xe0110' -> [(Abbreviation,[Ptr "VS33\0"#])]
  '\xe0111' -> [(Abbreviation,[Ptr "VS34\0"#])]
  '\xe0112' -> [(Abbreviation,[Ptr "VS35\0"#])]
  '\xe0113' -> [(Abbreviation,[Ptr "VS36\0"#])]
  '\xe0114' -> [(Abbreviation,[Ptr "VS37\0"#])]
  '\xe0115' -> [(Abbreviation,[Ptr "VS38\0"#])]
  '\xe0116' -> [(Abbreviation,[Ptr "VS39\0"#])]
  '\xe0117' -> [(Abbreviation,[Ptr "VS40\0"#])]
  '\xe0118' -> [(Abbreviation,[Ptr "VS41\0"#])]
  '\xe0119' -> [(Abbreviation,[Ptr "VS42\0"#])]
  '\xe011a' -> [(Abbreviation,[Ptr "VS43\0"#])]
  '\xe011b' -> [(Abbreviation,[Ptr "VS44\0"#])]
  '\xe011c' -> [(Abbreviation,[Ptr "VS45\0"#])]
  '\xe011d' -> [(Abbreviation,[Ptr "VS46\0"#])]
  '\xe011e' -> [(Abbreviation,[Ptr "VS47\0"#])]
  '\xe011f' -> [(Abbreviation,[Ptr "VS48\0"#])]
  '\xe0120' -> [(Abbreviation,[Ptr "VS49\0"#])]
  '\xe0121' -> [(Abbreviation,[Ptr "VS50\0"#])]
  '\xe0122' -> [(Abbreviation,[Ptr "VS51\0"#])]
  '\xe0123' -> [(Abbreviation,[Ptr "VS52\0"#])]
  '\xe0124' -> [(Abbreviation,[Ptr "VS53\0"#])]
  '\xe0125' -> [(Abbreviation,[Ptr "VS54\0"#])]
  '\xe0126' -> [(Abbreviation,[Ptr "VS55\0"#])]
  '\xe0127' -> [(Abbreviation,[Ptr "VS56\0"#])]
  '\xe0128' -> [(Abbreviation,[Ptr "VS57\0"#])]
  '\xe0129' -> [(Abbreviation,[Ptr "VS58\0"#])]
  '\xe012a' -> [(Abbreviation,[Ptr "VS59\0"#])]
  '\xe012b' -> [(Abbreviation,[Ptr "VS60\0"#])]
  '\xe012c' -> [(Abbreviation,[Ptr "VS61\0"#])]
  '\xe012d' -> [(Abbreviation,[Ptr "VS62\0"#])]
  '\xe012e' -> [(Abbreviation,[Ptr "VS63\0"#])]
  '\xe012f' -> [(Abbreviation,[Ptr "VS64\0"#])]
  '\xe0130' -> [(Abbreviation,[Ptr "VS65\0"#])]
  '\xe0131' -> [(Abbreviation,[Ptr "VS66\0"#])]
  '\xe0132' -> [(Abbreviation,[Ptr "VS67\0"#])]
  '\xe0133' -> [(Abbreviation,[Ptr "VS68\0"#])]
  '\xe0134' -> [(Abbreviation,[Ptr "VS69\0"#])]
  '\xe0135' -> [(Abbreviation,[Ptr "VS70\0"#])]
  '\xe0136' -> [(Abbreviation,[Ptr "VS71\0"#])]
  '\xe0137' -> [(Abbreviation,[Ptr "VS72\0"#])]
  '\xe0138' -> [(Abbreviation,[Ptr "VS73\0"#])]
  '\xe0139' -> [(Abbreviation,[Ptr "VS74\0"#])]
  '\xe013a' -> [(Abbreviation,[Ptr "VS75\0"#])]
  '\xe013b' -> [(Abbreviation,[Ptr "VS76\0"#])]
  '\xe013c' -> [(Abbreviation,[Ptr "VS77\0"#])]
  '\xe013d' -> [(Abbreviation,[Ptr "VS78\0"#])]
  '\xe013e' -> [(Abbreviation,[Ptr "VS79\0"#])]
  '\xe013f' -> [(Abbreviation,[Ptr "VS80\0"#])]
  '\xe0140' -> [(Abbreviation,[Ptr "VS81\0"#])]
  '\xe0141' -> [(Abbreviation,[Ptr "VS82\0"#])]
  '\xe0142' -> [(Abbreviation,[Ptr "VS83\0"#])]
  '\xe0143' -> [(Abbreviation,[Ptr "VS84\0"#])]
  '\xe0144' -> [(Abbreviation,[Ptr "VS85\0"#])]
  '\xe0145' -> [(Abbreviation,[Ptr "VS86\0"#])]
  '\xe0146' -> [(Abbreviation,[Ptr "VS87\0"#])]
  '\xe0147' -> [(Abbreviation,[Ptr "VS88\0"#])]
  '\xe0148' -> [(Abbreviation,[Ptr "VS89\0"#])]
  '\xe0149' -> [(Abbreviation,[Ptr "VS90\0"#])]
  '\xe014a' -> [(Abbreviation,[Ptr "VS91\0"#])]
  '\xe014b' -> [(Abbreviation,[Ptr "VS92\0"#])]
  '\xe014c' -> [(Abbreviation,[Ptr "VS93\0"#])]
  '\xe014d' -> [(Abbreviation,[Ptr "VS94\0"#])]
  '\xe014e' -> [(Abbreviation,[Ptr "VS95\0"#])]
  '\xe014f' -> [(Abbreviation,[Ptr "VS96\0"#])]
  '\xe0150' -> [(Abbreviation,[Ptr "VS97\0"#])]
  '\xe0151' -> [(Abbreviation,[Ptr "VS98\0"#])]
  '\xe0152' -> [(Abbreviation,[Ptr "VS99\0"#])]
  '\xe0153' -> [(Abbreviation,[Ptr "VS100\0"#])]
  '\xe0154' -> [(Abbreviation,[Ptr "VS101\0"#])]
  '\xe0155' -> [(Abbreviation,[Ptr "VS102\0"#])]
  '\xe0156' -> [(Abbreviation,[Ptr "VS103\0"#])]
  '\xe0157' -> [(Abbreviation,[Ptr "VS104\0"#])]
  '\xe0158' -> [(Abbreviation,[Ptr "VS105\0"#])]
  '\xe0159' -> [(Abbreviation,[Ptr "VS106\0"#])]
  '\xe015a' -> [(Abbreviation,[Ptr "VS107\0"#])]
  '\xe015b' -> [(Abbreviation,[Ptr "VS108\0"#])]
  '\xe015c' -> [(Abbreviation,[Ptr "VS109\0"#])]
  '\xe015d' -> [(Abbreviation,[Ptr "VS110\0"#])]
  '\xe015e' -> [(Abbreviation,[Ptr "VS111\0"#])]
  '\xe015f' -> [(Abbreviation,[Ptr "VS112\0"#])]
  '\xe0160' -> [(Abbreviation,[Ptr "VS113\0"#])]
  '\xe0161' -> [(Abbreviation,[Ptr "VS114\0"#])]
  '\xe0162' -> [(Abbreviation,[Ptr "VS115\0"#])]
  '\xe0163' -> [(Abbreviation,[Ptr "VS116\0"#])]
  '\xe0164' -> [(Abbreviation,[Ptr "VS117\0"#])]
  '\xe0165' -> [(Abbreviation,[Ptr "VS118\0"#])]
  '\xe0166' -> [(Abbreviation,[Ptr "VS119\0"#])]
  '\xe0167' -> [(Abbreviation,[Ptr "VS120\0"#])]
  '\xe0168' -> [(Abbreviation,[Ptr "VS121\0"#])]
  '\xe0169' -> [(Abbreviation,[Ptr "VS122\0"#])]
  '\xe016a' -> [(Abbreviation,[Ptr "VS123\0"#])]
  '\xe016b' -> [(Abbreviation,[Ptr "VS124\0"#])]
  '\xe016c' -> [(Abbreviation,[Ptr "VS125\0"#])]
  '\xe016d' -> [(Abbreviation,[Ptr "VS126\0"#])]
  '\xe016e' -> [(Abbreviation,[Ptr "VS127\0"#])]
  '\xe016f' -> [(Abbreviation,[Ptr "VS128\0"#])]
  '\xe0170' -> [(Abbreviation,[Ptr "VS129\0"#])]
  '\xe0171' -> [(Abbreviation,[Ptr "VS130\0"#])]
  '\xe0172' -> [(Abbreviation,[Ptr "VS131\0"#])]
  '\xe0173' -> [(Abbreviation,[Ptr "VS132\0"#])]
  '\xe0174' -> [(Abbreviation,[Ptr "VS133\0"#])]
  '\xe0175' -> [(Abbreviation,[Ptr "VS134\0"#])]
  '\xe0176' -> [(Abbreviation,[Ptr "VS135\0"#])]
  '\xe0177' -> [(Abbreviation,[Ptr "VS136\0"#])]
  '\xe0178' -> [(Abbreviation,[Ptr "VS137\0"#])]
  '\xe0179' -> [(Abbreviation,[Ptr "VS138\0"#])]
  '\xe017a' -> [(Abbreviation,[Ptr "VS139\0"#])]
  '\xe017b' -> [(Abbreviation,[Ptr "VS140\0"#])]
  '\xe017c' -> [(Abbreviation,[Ptr "VS141\0"#])]
  '\xe017d' -> [(Abbreviation,[Ptr "VS142\0"#])]
  '\xe017e' -> [(Abbreviation,[Ptr "VS143\0"#])]
  '\xe017f' -> [(Abbreviation,[Ptr "VS144\0"#])]
  '\xe0180' -> [(Abbreviation,[Ptr "VS145\0"#])]
  '\xe0181' -> [(Abbreviation,[Ptr "VS146\0"#])]
  '\xe0182' -> [(Abbreviation,[Ptr "VS147\0"#])]
  '\xe0183' -> [(Abbreviation,[Ptr "VS148\0"#])]
  '\xe0184' -> [(Abbreviation,[Ptr "VS149\0"#])]
  '\xe0185' -> [(Abbreviation,[Ptr "VS150\0"#])]
  '\xe0186' -> [(Abbreviation,[Ptr "VS151\0"#])]
  '\xe0187' -> [(Abbreviation,[Ptr "VS152\0"#])]
  '\xe0188' -> [(Abbreviation,[Ptr "VS153\0"#])]
  '\xe0189' -> [(Abbreviation,[Ptr "VS154\0"#])]
  '\xe018a' -> [(Abbreviation,[Ptr "VS155\0"#])]
  '\xe018b' -> [(Abbreviation,[Ptr "VS156\0"#])]
  '\xe018c' -> [(Abbreviation,[Ptr "VS157\0"#])]
  '\xe018d' -> [(Abbreviation,[Ptr "VS158\0"#])]
  '\xe018e' -> [(Abbreviation,[Ptr "VS159\0"#])]
  '\xe018f' -> [(Abbreviation,[Ptr "VS160\0"#])]
  '\xe0190' -> [(Abbreviation,[Ptr "VS161\0"#])]
  '\xe0191' -> [(Abbreviation,[Ptr "VS162\0"#])]
  '\xe0192' -> [(Abbreviation,[Ptr "VS163\0"#])]
  '\xe0193' -> [(Abbreviation,[Ptr "VS164\0"#])]
  '\xe0194' -> [(Abbreviation,[Ptr "VS165\0"#])]
  '\xe0195' -> [(Abbreviation,[Ptr "VS166\0"#])]
  '\xe0196' -> [(Abbreviation,[Ptr "VS167\0"#])]
  '\xe0197' -> [(Abbreviation,[Ptr "VS168\0"#])]
  '\xe0198' -> [(Abbreviation,[Ptr "VS169\0"#])]
  '\xe0199' -> [(Abbreviation,[Ptr "VS170\0"#])]
  '\xe019a' -> [(Abbreviation,[Ptr "VS171\0"#])]
  '\xe019b' -> [(Abbreviation,[Ptr "VS172\0"#])]
  '\xe019c' -> [(Abbreviation,[Ptr "VS173\0"#])]
  '\xe019d' -> [(Abbreviation,[Ptr "VS174\0"#])]
  '\xe019e' -> [(Abbreviation,[Ptr "VS175\0"#])]
  '\xe019f' -> [(Abbreviation,[Ptr "VS176\0"#])]
  '\xe01a0' -> [(Abbreviation,[Ptr "VS177\0"#])]
  '\xe01a1' -> [(Abbreviation,[Ptr "VS178\0"#])]
  '\xe01a2' -> [(Abbreviation,[Ptr "VS179\0"#])]
  '\xe01a3' -> [(Abbreviation,[Ptr "VS180\0"#])]
  '\xe01a4' -> [(Abbreviation,[Ptr "VS181\0"#])]
  '\xe01a5' -> [(Abbreviation,[Ptr "VS182\0"#])]
  '\xe01a6' -> [(Abbreviation,[Ptr "VS183\0"#])]
  '\xe01a7' -> [(Abbreviation,[Ptr "VS184\0"#])]
  '\xe01a8' -> [(Abbreviation,[Ptr "VS185\0"#])]
  '\xe01a9' -> [(Abbreviation,[Ptr "VS186\0"#])]
  '\xe01aa' -> [(Abbreviation,[Ptr "VS187\0"#])]
  '\xe01ab' -> [(Abbreviation,[Ptr "VS188\0"#])]
  '\xe01ac' -> [(Abbreviation,[Ptr "VS189\0"#])]
  '\xe01ad' -> [(Abbreviation,[Ptr "VS190\0"#])]
  '\xe01ae' -> [(Abbreviation,[Ptr "VS191\0"#])]
  '\xe01af' -> [(Abbreviation,[Ptr "VS192\0"#])]
  '\xe01b0' -> [(Abbreviation,[Ptr "VS193\0"#])]
  '\xe01b1' -> [(Abbreviation,[Ptr "VS194\0"#])]
  '\xe01b2' -> [(Abbreviation,[Ptr "VS195\0"#])]
  '\xe01b3' -> [(Abbreviation,[Ptr "VS196\0"#])]
  '\xe01b4' -> [(Abbreviation,[Ptr "VS197\0"#])]
  '\xe01b5' -> [(Abbreviation,[Ptr "VS198\0"#])]
  '\xe01b6' -> [(Abbreviation,[Ptr "VS199\0"#])]
  '\xe01b7' -> [(Abbreviation,[Ptr "VS200\0"#])]
  '\xe01b8' -> [(Abbreviation,[Ptr "VS201\0"#])]
  '\xe01b9' -> [(Abbreviation,[Ptr "VS202\0"#])]
  '\xe01ba' -> [(Abbreviation,[Ptr "VS203\0"#])]
  '\xe01bb' -> [(Abbreviation,[Ptr "VS204\0"#])]
  '\xe01bc' -> [(Abbreviation,[Ptr "VS205\0"#])]
  '\xe01bd' -> [(Abbreviation,[Ptr "VS206\0"#])]
  '\xe01be' -> [(Abbreviation,[Ptr "VS207\0"#])]
  '\xe01bf' -> [(Abbreviation,[Ptr "VS208\0"#])]
  '\xe01c0' -> [(Abbreviation,[Ptr "VS209\0"#])]
  '\xe01c1' -> [(Abbreviation,[Ptr "VS210\0"#])]
  '\xe01c2' -> [(Abbreviation,[Ptr "VS211\0"#])]
  '\xe01c3' -> [(Abbreviation,[Ptr "VS212\0"#])]
  '\xe01c4' -> [(Abbreviation,[Ptr "VS213\0"#])]
  '\xe01c5' -> [(Abbreviation,[Ptr "VS214\0"#])]
  '\xe01c6' -> [(Abbreviation,[Ptr "VS215\0"#])]
  '\xe01c7' -> [(Abbreviation,[Ptr "VS216\0"#])]
  '\xe01c8' -> [(Abbreviation,[Ptr "VS217\0"#])]
  '\xe01c9' -> [(Abbreviation,[Ptr "VS218\0"#])]
  '\xe01ca' -> [(Abbreviation,[Ptr "VS219\0"#])]
  '\xe01cb' -> [(Abbreviation,[Ptr "VS220\0"#])]
  '\xe01cc' -> [(Abbreviation,[Ptr "VS221\0"#])]
  '\xe01cd' -> [(Abbreviation,[Ptr "VS222\0"#])]
  '\xe01ce' -> [(Abbreviation,[Ptr "VS223\0"#])]
  '\xe01cf' -> [(Abbreviation,[Ptr "VS224\0"#])]
  '\xe01d0' -> [(Abbreviation,[Ptr "VS225\0"#])]
  '\xe01d1' -> [(Abbreviation,[Ptr "VS226\0"#])]
  '\xe01d2' -> [(Abbreviation,[Ptr "VS227\0"#])]
  '\xe01d3' -> [(Abbreviation,[Ptr "VS228\0"#])]
  '\xe01d4' -> [(Abbreviation,[Ptr "VS229\0"#])]
  '\xe01d5' -> [(Abbreviation,[Ptr "VS230\0"#])]
  '\xe01d6' -> [(Abbreviation,[Ptr "VS231\0"#])]
  '\xe01d7' -> [(Abbreviation,[Ptr "VS232\0"#])]
  '\xe01d8' -> [(Abbreviation,[Ptr "VS233\0"#])]
  '\xe01d9' -> [(Abbreviation,[Ptr "VS234\0"#])]
  '\xe01da' -> [(Abbreviation,[Ptr "VS235\0"#])]
  '\xe01db' -> [(Abbreviation,[Ptr "VS236\0"#])]
  '\xe01dc' -> [(Abbreviation,[Ptr "VS237\0"#])]
  '\xe01dd' -> [(Abbreviation,[Ptr "VS238\0"#])]
  '\xe01de' -> [(Abbreviation,[Ptr "VS239\0"#])]
  '\xe01df' -> [(Abbreviation,[Ptr "VS240\0"#])]
  '\xe01e0' -> [(Abbreviation,[Ptr "VS241\0"#])]
  '\xe01e1' -> [(Abbreviation,[Ptr "VS242\0"#])]
  '\xe01e2' -> [(Abbreviation,[Ptr "VS243\0"#])]
  '\xe01e3' -> [(Abbreviation,[Ptr "VS244\0"#])]
  '\xe01e4' -> [(Abbreviation,[Ptr "VS245\0"#])]
  '\xe01e5' -> [(Abbreviation,[Ptr "VS246\0"#])]
  '\xe01e6' -> [(Abbreviation,[Ptr "VS247\0"#])]
  '\xe01e7' -> [(Abbreviation,[Ptr "VS248\0"#])]
  '\xe01e8' -> [(Abbreviation,[Ptr "VS249\0"#])]
  '\xe01e9' -> [(Abbreviation,[Ptr "VS250\0"#])]
  '\xe01ea' -> [(Abbreviation,[Ptr "VS251\0"#])]
  '\xe01eb' -> [(Abbreviation,[Ptr "VS252\0"#])]
  '\xe01ec' -> [(Abbreviation,[Ptr "VS253\0"#])]
  '\xe01ed' -> [(Abbreviation,[Ptr "VS254\0"#])]
  '\xe01ee' -> [(Abbreviation,[Ptr "VS255\0"#])]
  '\xe01ef' -> [(Abbreviation,[Ptr "VS256\0"#])]

  _ -> mempty
