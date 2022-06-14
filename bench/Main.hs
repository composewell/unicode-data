import Control.DeepSeq (NFData, deepseq)
import Data.Ix (Ix(..))
import Test.Tasty.Bench (Benchmark, bgroup, bench, bcompare, nf, defaultMain)

import qualified Data.Char as B
import qualified Unicode.Char.Case as C
import qualified Unicode.Char.Case.Compat as CC
import qualified Unicode.Char.General as G
import qualified Unicode.Char.General.Compat as GC
import qualified Unicode.Char.Identifiers as I
import qualified Unicode.Char.Normalization as N
import qualified Unicode.Char.Numeric as Num
import qualified Unicode.Char.Numeric.Compat as NumCompat

-- | A unit benchmark
data Bench a = Bench
  { _title :: !String  -- ^ Name
  , _func :: Char -> a -- ^ Function to benchmark
  }

validChars :: [Char]
validChars = map B.chr $ concat
    [ [0x0000..0x007F    ]
    , [0x0080..0x00FF    ]
    , [0x0100..0x017F    ]
    , [0x0180..0x024F    ]
    , [0x0250..0x02AF    ]
    , [0x02B0..0x02FF    ]
    , [0x0300..0x036F    ]
    , [0x0370..0x03FF    ]
    , [0x0400..0x04FF    ]
    , [0x0500..0x052F    ]
    , [0x0530..0x058F    ]
    , [0x0590..0x05FF    ]
    , [0x0600..0x06FF    ]
    , [0x0700..0x074F    ]
    , [0x0750..0x077F    ]
    , [0x0780..0x07BF    ]
    , [0x07C0..0x07FF    ]
    , [0x0800..0x083F    ]
    , [0x0840..0x085F    ]
    , [0x0860..0x086F    ]
    , [0x0870..0x089F    ]
    , [0x08A0..0x08FF    ]
    , [0x0900..0x097F    ]
    , [0x0980..0x09FF    ]
    , [0x0A00..0x0A7F    ]
    , [0x0A80..0x0AFF    ]
    , [0x0B00..0x0B7F    ]
    , [0x0B80..0x0BFF    ]
    , [0x0C00..0x0C7F    ]
    , [0x0C80..0x0CFF    ]
    , [0x0D00..0x0D7F    ]
    , [0x0D80..0x0DFF    ]
    , [0x0E00..0x0E7F    ]
    , [0x0E80..0x0EFF    ]
    , [0x0F00..0x0FFF    ]
    , [0x1000..0x109F    ]
    , [0x10A0..0x10FF    ]
    , [0x1100..0x11FF    ]
    , [0x1200..0x137F    ]
    , [0x1380..0x139F    ]
    , [0x13A0..0x13FF    ]
    , [0x1400..0x167F    ]
    , [0x1680..0x169F    ]
    , [0x16A0..0x16FF    ]
    , [0x1700..0x171F    ]
    , [0x1720..0x173F    ]
    , [0x1740..0x175F    ]
    , [0x1760..0x177F    ]
    , [0x1780..0x17FF    ]
    , [0x1800..0x18AF    ]
    , [0x18B0..0x18FF    ]
    , [0x1900..0x194F    ]
    , [0x1950..0x197F    ]
    , [0x1980..0x19DF    ]
    , [0x19E0..0x19FF    ]
    , [0x1A00..0x1A1F    ]
    , [0x1A20..0x1AAF    ]
    , [0x1AB0..0x1AFF    ]
    , [0x1B00..0x1B7F    ]
    , [0x1B80..0x1BBF    ]
    , [0x1BC0..0x1BFF    ]
    , [0x1C00..0x1C4F    ]
    , [0x1C50..0x1C7F    ]
    , [0x1C80..0x1C8F    ]
    , [0x1C90..0x1CBF    ]
    , [0x1CC0..0x1CCF    ]
    , [0x1CD0..0x1CFF    ]
    , [0x1D00..0x1D7F    ]
    , [0x1D80..0x1DBF    ]
    , [0x1DC0..0x1DFF    ]
    , [0x1E00..0x1EFF    ]
    , [0x1F00..0x1FFF    ]
    , [0x2000..0x206F    ]
    , [0x2070..0x209F    ]
    , [0x20A0..0x20CF    ]
    , [0x20D0..0x20FF    ]
    , [0x2100..0x214F    ]
    , [0x2150..0x218F    ]
    , [0x2190..0x21FF    ]
    , [0x2200..0x22FF    ]
    , [0x2300..0x23FF    ]
    , [0x2400..0x243F    ]
    , [0x2440..0x245F    ]
    , [0x2460..0x24FF    ]
    , [0x2500..0x257F    ]
    , [0x2580..0x259F    ]
    , [0x25A0..0x25FF    ]
    , [0x2600..0x26FF    ]
    , [0x2700..0x27BF    ]
    , [0x27C0..0x27EF    ]
    , [0x27F0..0x27FF    ]
    , [0x2800..0x28FF    ]
    , [0x2900..0x297F    ]
    , [0x2980..0x29FF    ]
    , [0x2A00..0x2AFF    ]
    , [0x2B00..0x2BFF    ]
    , [0x2C00..0x2C5F    ]
    , [0x2C60..0x2C7F    ]
    , [0x2C80..0x2CFF    ]
    , [0x2D00..0x2D2F    ]
    , [0x2D30..0x2D7F    ]
    , [0x2D80..0x2DDF    ]
    , [0x2DE0..0x2DFF    ]
    , [0x2E00..0x2E7F    ]
    , [0x2E80..0x2EFF    ]
    , [0x2F00..0x2FDF    ]
    , [0x2FF0..0x2FFF    ]
    , [0x3000..0x303F    ]
    , [0x3040..0x309F    ]
    , [0x30A0..0x30FF    ]
    , [0x3100..0x312F    ]
    , [0x3130..0x318F    ]
    , [0x3190..0x319F    ]
    , [0x31A0..0x31BF    ]
    , [0x31C0..0x31EF    ]
    , [0x31F0..0x31FF    ]
    , [0x3200..0x32FF    ]
    , [0x3300..0x33FF    ]
    , [0x3400..0x4DBF    ]
    , [0x4DC0..0x4DFF    ]
    , [0x4E00..0x9FFF    ]
    , [0xA000..0xA48F    ]
    , [0xA490..0xA4CF    ]
    , [0xA4D0..0xA4FF    ]
    , [0xA500..0xA63F    ]
    , [0xA640..0xA69F    ]
    , [0xA6A0..0xA6FF    ]
    , [0xA700..0xA71F    ]
    , [0xA720..0xA7FF    ]
    , [0xA800..0xA82F    ]
    , [0xA830..0xA83F    ]
    , [0xA840..0xA87F    ]
    , [0xA880..0xA8DF    ]
    , [0xA8E0..0xA8FF    ]
    , [0xA900..0xA92F    ]
    , [0xA930..0xA95F    ]
    , [0xA960..0xA97F    ]
    , [0xA980..0xA9DF    ]
    , [0xA9E0..0xA9FF    ]
    , [0xAA00..0xAA5F    ]
    , [0xAA60..0xAA7F    ]
    , [0xAA80..0xAADF    ]
    , [0xAAE0..0xAAFF    ]
    , [0xAB00..0xAB2F    ]
    , [0xAB30..0xAB6F    ]
    , [0xAB70..0xABBF    ]
    , [0xABC0..0xABFF    ]
    , [0xAC00..0xD7AF    ]
    , [0xD7B0..0xD7FF    ]
    , [0xD800..0xDB7F    ]
    , [0xDB80..0xDBFF    ]
    , [0xDC00..0xDFFF    ]
    , [0xE000..0xF8FF    ]
    , [0xF900..0xFAFF    ]
    , [0xFB00..0xFB4F    ]
    , [0xFB50..0xFDFF    ]
    , [0xFE00..0xFE0F    ]
    , [0xFE10..0xFE1F    ]
    , [0xFE20..0xFE2F    ]
    , [0xFE30..0xFE4F    ]
    , [0xFE50..0xFE6F    ]
    , [0xFE70..0xFEFF    ]
    , [0xFF00..0xFFEF    ]
    , [0xFFF0..0xFFFF    ]
    , [0x10000..0x1007F  ]
    , [0x10080..0x100FF  ]
    , [0x10100..0x1013F  ]
    , [0x10140..0x1018F  ]
    , [0x10190..0x101CF  ]
    , [0x101D0..0x101FF  ]
    , [0x10280..0x1029F  ]
    , [0x102A0..0x102DF  ]
    , [0x102E0..0x102FF  ]
    , [0x10300..0x1032F  ]
    , [0x10330..0x1034F  ]
    , [0x10350..0x1037F  ]
    , [0x10380..0x1039F  ]
    , [0x103A0..0x103DF  ]
    , [0x10400..0x1044F  ]
    , [0x10450..0x1047F  ]
    , [0x10480..0x104AF  ]
    , [0x104B0..0x104FF  ]
    , [0x10500..0x1052F  ]
    , [0x10530..0x1056F  ]
    , [0x10570..0x105BF  ]
    , [0x10600..0x1077F  ]
    , [0x10780..0x107BF  ]
    , [0x10800..0x1083F  ]
    , [0x10840..0x1085F  ]
    , [0x10860..0x1087F  ]
    , [0x10880..0x108AF  ]
    , [0x108E0..0x108FF  ]
    , [0x10900..0x1091F  ]
    , [0x10920..0x1093F  ]
    , [0x10980..0x1099F  ]
    , [0x109A0..0x109FF  ]
    , [0x10A00..0x10A5F  ]
    , [0x10A60..0x10A7F  ]
    , [0x10A80..0x10A9F  ]
    , [0x10AC0..0x10AFF  ]
    , [0x10B00..0x10B3F  ]
    , [0x10B40..0x10B5F  ]
    , [0x10B60..0x10B7F  ]
    , [0x10B80..0x10BAF  ]
    , [0x10C00..0x10C4F  ]
    , [0x10C80..0x10CFF  ]
    , [0x10D00..0x10D3F  ]
    , [0x10E60..0x10E7F  ]
    , [0x10E80..0x10EBF  ]
    , [0x10F00..0x10F2F  ]
    , [0x10F30..0x10F6F  ]
    , [0x10F70..0x10FAF  ]
    , [0x10FB0..0x10FDF  ]
    , [0x10FE0..0x10FFF  ]
    , [0x11000..0x1107F  ]
    , [0x11080..0x110CF  ]
    , [0x110D0..0x110FF  ]
    , [0x11100..0x1114F  ]
    , [0x11150..0x1117F  ]
    , [0x11180..0x111DF  ]
    , [0x111E0..0x111FF  ]
    , [0x11200..0x1124F  ]
    , [0x11280..0x112AF  ]
    , [0x112B0..0x112FF  ]
    , [0x11300..0x1137F  ]
    , [0x11400..0x1147F  ]
    , [0x11480..0x114DF  ]
    , [0x11580..0x115FF  ]
    , [0x11600..0x1165F  ]
    , [0x11660..0x1167F  ]
    , [0x11680..0x116CF  ]
    , [0x11700..0x1174F  ]
    , [0x11800..0x1184F  ]
    , [0x118A0..0x118FF  ]
    , [0x11900..0x1195F  ]
    , [0x119A0..0x119FF  ]
    , [0x11A00..0x11A4F  ]
    , [0x11A50..0x11AAF  ]
    , [0x11AB0..0x11ABF  ]
    , [0x11AC0..0x11AFF  ]
    , [0x11C00..0x11C6F  ]
    , [0x11C70..0x11CBF  ]
    , [0x11D00..0x11D5F  ]
    , [0x11D60..0x11DAF  ]
    , [0x11EE0..0x11EFF  ]
    , [0x11FB0..0x11FBF  ]
    , [0x11FC0..0x11FFF  ]
    , [0x12000..0x123FF  ]
    , [0x12400..0x1247F  ]
    , [0x12480..0x1254F  ]
    , [0x12F90..0x12FFF  ]
    , [0x13000..0x1342F  ]
    , [0x13430..0x1343F  ]
    , [0x14400..0x1467F  ]
    , [0x16800..0x16A3F  ]
    , [0x16A40..0x16A6F  ]
    , [0x16A70..0x16ACF  ]
    , [0x16AD0..0x16AFF  ]
    , [0x16B00..0x16B8F  ]
    , [0x16E40..0x16E9F  ]
    , [0x16F00..0x16F9F  ]
    , [0x16FE0..0x16FFF  ]
    , [0x17000..0x187FF  ]
    , [0x18800..0x18AFF  ]
    , [0x18B00..0x18CFF  ]
    , [0x18D00..0x18D7F  ]
    , [0x1AFF0..0x1AFFF  ]
    , [0x1B000..0x1B0FF  ]
    , [0x1B100..0x1B12F  ]
    , [0x1B130..0x1B16F  ]
    , [0x1B170..0x1B2FF  ]
    , [0x1BC00..0x1BC9F  ]
    , [0x1BCA0..0x1BCAF  ]
    , [0x1CF00..0x1CFCF  ]
    , [0x1D000..0x1D0FF  ]
    , [0x1D100..0x1D1FF  ]
    , [0x1D200..0x1D24F  ]
    , [0x1D2E0..0x1D2FF  ]
    , [0x1D300..0x1D35F  ]
    , [0x1D360..0x1D37F  ]
    , [0x1D400..0x1D7FF  ]
    , [0x1D800..0x1DAAF  ]
    , [0x1DF00..0x1DFFF  ]
    , [0x1E000..0x1E02F  ]
    , [0x1E100..0x1E14F  ]
    , [0x1E290..0x1E2BF  ]
    , [0x1E2C0..0x1E2FF  ]
    , [0x1E7E0..0x1E7FF  ]
    , [0x1E800..0x1E8DF  ]
    , [0x1E900..0x1E95F  ]
    , [0x1EC70..0x1ECBF  ]
    , [0x1ED00..0x1ED4F  ]
    , [0x1EE00..0x1EEFF  ]
    , [0x1F000..0x1F02F  ]
    , [0x1F030..0x1F09F  ]
    , [0x1F0A0..0x1F0FF  ]
    , [0x1F100..0x1F1FF  ]
    , [0x1F200..0x1F2FF  ]
    , [0x1F300..0x1F5FF  ]
    , [0x1F600..0x1F64F  ]
    , [0x1F650..0x1F67F  ]
    , [0x1F680..0x1F6FF  ]
    , [0x1F700..0x1F77F  ]
    , [0x1F780..0x1F7FF  ]
    , [0x1F800..0x1F8FF  ]
    , [0x1F900..0x1F9FF  ]
    , [0x1FA00..0x1FA6F  ]
    , [0x1FA70..0x1FAFF  ]
    , [0x1FB00..0x1FBFF  ]
    , [0x20000..0x2A6DF  ]
    , [0x2A700..0x2B73F  ]
    , [0x2B740..0x2B81F  ]
    , [0x2B820..0x2CEAF  ]
    , [0x2CEB0..0x2EBEF  ]
    , [0x2F800..0x2FA1F  ]
    , [0x30000..0x3134F  ]
    , [0xE0000..0xE007F  ]
    , [0xE0100..0xE01EF  ]
    , [0xF0000..0xFFFFF  ]
    , [0x100000..0x10FFFF]
    ]

main :: IO ()
main = defaultMain
  [ bgroup "Unicode.Char.Case"
    [ bgroup "isLowerCase"
      [ benchNF "unicode-data" C.isLowerCase
      ]
    , bgroup "isUpperCase"
      [ benchNF "unicode-data" C.isUpperCase
      ]
    ]
  , bgroup "Unicode.Char.Case.Compat"
    [ bgroup' "isLower"
      [ Bench "base"         B.isLower
      , Bench "unicode-data" CC.isLower
      ]
    , bgroup' "isUpper"
      [ Bench "base"         B.isUpper
      , Bench "unicode-data" CC.isUpper
      ]
    , bgroup' "toLower"
      [ Bench "base"         B.toLower
      , Bench "unicode-data" CC.toLower
      ]
    , bgroup' "toTitle"
      [ Bench "base"         B.toTitle
      , Bench "unicode-data" CC.toTitle
      ]
    , bgroup' "toUpper"
      [ Bench "base"         B.toUpper
      , Bench "unicode-data" CC.toUpper
      ]
    ]
  , bgroup "Unicode.Char.General"
    -- Character classification
    [ bgroup' "generalCategory"
      [ Bench "base"          (show . B.generalCategory)
      , Bench "unicode-data"  (show . G.generalCategory)
      ]
    , bgroup "isAlphabetic"
      [ benchNF "unicode-data"  G.isAlphabetic
      ]
    , bgroup' "isAlphaNum"
      [ Bench "base"          B.isAlphaNum
      , Bench "unicode-data"  G.isAlphaNum
      ]
    , bgroup' "isControl"
      [ Bench "base"          B.isControl
      , Bench "unicode-data"  G.isControl
      ]
    , bgroup' "isMark"
      [ Bench "base"          B.isMark
      , Bench "unicode-data"  G.isMark
      ]
    , bgroup' "isPrint"
      [ Bench "base"          B.isPrint
      , Bench "unicode-data"  G.isPrint
      ]
    , bgroup' "isPunctuation"
      [ Bench "base"          B.isPunctuation
      , Bench "unicode-data"  G.isPunctuation
      ]
    , bgroup' "isSeparator"
      [ Bench "base"          B.isSeparator
      , Bench "unicode-data"  G.isSeparator
      ]
    , bgroup' "isSymbol"
      [ Bench "base"          B.isSymbol
      , Bench "unicode-data"  G.isSymbol
      ]
    , bgroup "isWhiteSpace"
      [ benchNF "unicode-data"  G.isWhiteSpace
      ]
    -- Korean Hangul Characters
    , bgroup "isHangul"
      [ benchNF "unicode-data"  G.isHangul
      ]
    , bgroup "isHangulLV"
      [ benchNF "unicode-data"  G.isHangul
      ]
    , bgroup "isJamo"
      [ benchNF "unicode-data"  G.isJamo
      ]
    , bgroup "jamoLIndex"
      [ benchNF "unicode-data"  G.jamoLIndex
      ]
    , bgroup "jamoVIndex"
      [ benchNF "unicode-data"  G.jamoVIndex
      ]
    , bgroup "jamoTIndex"
      [ benchNF "unicode-data"  G.jamoTIndex
      ]
    ]
  , bgroup "Unicode.Char.General.Compat"
    [ bgroup' "isAlpha"
      [ Bench "base"          B.isAlpha
      , Bench "unicode-data"  GC.isAlpha
      ]
    , bgroup' "isLetter"
      [ Bench "base"          B.isLetter
      , Bench "unicode-data"  GC.isLetter
      ]
    , bgroup' "isSpace"
      [ Bench "base"          B.isSpace
      , Bench "unicode-data"  GC.isSpace
      ]
    ]
  , bgroup "Unicode.Char.Identifiers"
    [ bgroup "isIDContinue"
      [ benchNF "unicode-data"  I.isIDContinue
      ]
    , bgroup "isIDStart"
      [ benchNF "unicode-data"  I.isIDStart
      ]
    , bgroup "isXIDContinue"
      [ benchNF "unicode-data"  I.isXIDContinue
      ]
    , bgroup "isXIDStart"
      [ benchNF "unicode-data"  I.isXIDStart
      ]
    , bgroup "isPatternSyntax"
      [ benchNF "unicode-data"  I.isPatternSyntax
      ]
    , bgroup "isPatternWhitespace"
      [ benchNF "unicode-data"  I.isPatternWhitespace
      ]
    ]
  , bgroup "Unicode.Char.Normalization"
    [ bgroup "isCombining"
      [ benchNF "unicode-data"  N.isCombining
      ]
    , bgroup "combiningClass"
      [ benchNF "unicode-data"  N.combiningClass
      ]
    , bgroup "isCombiningStarter"
      [ benchNF "unicode-data"  N.isCombiningStarter
      ]
    -- [TODO] compose, composeStarters
    , bgroup "isDecomposable"
      [ bgroup "Canonical"
        [ benchNF "unicode-data" (N.isDecomposable N.Canonical)
        ]
      , bgroup "Kompat"
        [ benchNF "unicode-data" (N.isDecomposable N.Kompat)
        ]
      ]
    -- [FIXME] Fail due to non-exhaustive pattern matching
    -- , bgroup "decompose"
    --   [ bgroup "Canonical"
    --     [ benchNF "unicode-data" (N.decompose N.Canonical)
    --     ]
    --   , bgroup "Kompat"
    --     [ benchNF "unicode-data" (N.decompose N.Kompat)
    --     ]
    --   ]
    , bgroup "decomposeHangul"
      [ benchNF "unicode-data" N.decomposeHangul
      ]
    ]
  , bgroup "Unicode.Char.Numeric"
    -- [TODO] Replace with 'isNumber' once the migration is done.
    [ bgroup "isNumeric"
      [ benchNF "unicode-data"  Num.isNumeric
      ]
    , bgroup "numericValue"
      [ benchNF "unicode-data" Num.numericValue
      ]
    , bgroup "integerValue"
      [ benchNF "unicode-data" Num.integerValue
      ]
    ]
  , bgroup "Unicode.Char.Numeric.Compat"
    [ bgroup' "isNumber"
      [ Bench "base"          B.isNumber
      , Bench "unicode-data"  NumCompat.isNumber
      ]
    ]
  ]
  where
    bgroup' groupTitle bs = bgroup groupTitle
      [ benchNF' groupTitle title f
      | Bench title f <- bs
      ]

    -- [NOTE] Works if groupTitle uniquely identifies the benchmark group.
    benchNF' groupTitle title = case title of
      "base" -> benchNF title
      _      -> bcompare ("$NF == \"base\" && $(NF-1) == \"" ++ groupTitle ++ "\"")
              . benchNF title

    benchNF :: forall a. (NFData a) => String -> (Char -> a) -> Benchmark
    benchNF t f = bench t $ nf (fold_ f) validChars

    fold_ :: forall a. (NFData a) => (Char -> a) -> [Char] -> ()
    fold_ f = foldr (deepseq . f) ()
