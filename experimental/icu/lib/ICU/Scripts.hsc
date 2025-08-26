{-# LANGUAGE ForeignFunctionInterface #-}

module ICU.Scripts
    ( Script(..)
    , script
    , codepointScript
    , scriptShortName
    , scriptExtensions
    , scriptExtensionsRaw
    ) where

import Control.Monad ((>=>))
import Data.Char (ord)
import Data.Int (Int32)
import qualified Data.List.NonEmpty as NE
import Data.Word (Word32)
import Foreign (Ptr)
import Foreign.C (CInt(..))
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Array (allocaArray, peekArray)
-- import Foreign.Marshal.Utils (with)
import System.IO.Unsafe (unsafePerformIO)

type UChar32 = Word32
type UScriptCode = CInt
-- type UErrorCode = Int

-- [NOTE] Faster to skip the error code. We should not need it anyway,
--        as we always use valid codepoints.
-- foreign import ccall unsafe "icu.h __hs_uscript_getScript" uscript_getScript
--     :: UChar32 -> Ptr UErrorCode -> IO UScriptCode
foreign import ccall safe "icu.h __hs_uscript_getScript" uscript_getScript
    :: UChar32 -> UScriptCode

foreign import ccall unsafe "icu.h __hs_uscript_getScriptExtensions" uscript_getScriptExtensions
    :: UChar32 -> Ptr UScriptCode -> Int32 -> IO Int32

foreign import ccall unsafe "icu.h __hs_uscript_getShortName" uscript_getShortName
    :: UScriptCode -> IO CString

{-# INLINE codepointScript #-}
codepointScript :: Word32 -> Script
-- codepointScript = toEnum . unsafePerformIO . with 0 . uscript_getScript
codepointScript = toEnum . fromIntegral . uscript_getScript

{-# INLINE script #-}
script :: Char -> Script
script = toEnum . fromIntegral . uscript_getScript . fromIntegral . ord

scriptExtensions :: Char -> NE.NonEmpty Script
scriptExtensions
    = NE.fromList
    . fmap (toEnum . fromIntegral)
    . scriptExtensionsRaw

scriptExtensionsRaw :: Char -> [UScriptCode]
scriptExtensionsRaw
    = unsafePerformIO
    . (\cp -> allocaArray capacity
        (\ptr -> uscript_getScriptExtensions cp ptr (fromIntegral capacity)
             >>= ((`peekArray` ptr) . fromIntegral)))
    . fromIntegral
    . ord
    where
    capacity = 30

scriptShortName :: Script -> String
scriptShortName
    = unsafePerformIO
    . (uscript_getShortName . fromIntegral . fromEnum >=> peekCString)


-- See: https://unicode-org.github.io/icu-docs/apidoc/released/icu4c/uscript_8h_source.html
-- Last sync: 2023-03-09

data Script
    = Common                      -- ^ USCRIPT_COMMON                        =  0
    | Inherited                   -- ^ USCRIPT_INHERITED                     =  1
    | Arabic                      -- ^ USCRIPT_ARABIC                        =  2
    | Armenian                    -- ^ USCRIPT_ARMENIAN                      =  3
    | Bengali                     -- ^ USCRIPT_BENGALI                       =  4
    | Bopomofo                    -- ^ USCRIPT_BOPOMOFO                      =  5
    | Cherokee                    -- ^ USCRIPT_CHEROKEE                      =  6
    | Coptic                      -- ^ USCRIPT_COPTIC                        =  7
    | Cyrillic                    -- ^ USCRIPT_CYRILLIC                      =  8
    | Deseret                     -- ^ USCRIPT_DESERET                       =  9
    | Devanagari                  -- ^ USCRIPT_DEVANAGARI                    = 10
    | Ethiopic                    -- ^ USCRIPT_ETHIOPIC                      = 11
    | Georgian                    -- ^ USCRIPT_GEORGIAN                      = 12
    | Gothic                      -- ^ USCRIPT_GOTHIC                        = 13
    | Greek                       -- ^ USCRIPT_GREEK                         = 14
    | Gujarati                    -- ^ USCRIPT_GUJARATI                      = 15
    | Gurmukhi                    -- ^ USCRIPT_GURMUKHI                      = 16
    | Han                         -- ^ USCRIPT_HAN                           = 17
    | Hangul                      -- ^ USCRIPT_HANGUL                        = 18
    | Hebrew                      -- ^ USCRIPT_HEBREW                        = 19
    | Hiragana                    -- ^ USCRIPT_HIRAGANA                      = 20
    | Kannada                     -- ^ USCRIPT_KANNADA                       = 21
    | Katakana                    -- ^ USCRIPT_KATAKANA                      = 22
    | Khmer                       -- ^ USCRIPT_KHMER                         = 23
    | Lao                         -- ^ USCRIPT_LAO                           = 24
    | Latin                       -- ^ USCRIPT_LATIN                         = 25
    | Malayalam                   -- ^ USCRIPT_MALAYALAM                     = 26
    | Mongolian                   -- ^ USCRIPT_MONGOLIAN                     = 27
    | Myanmar                     -- ^ USCRIPT_MYANMAR                       = 28
    | Ogham                       -- ^ USCRIPT_OGHAM                         = 29
    | OldItalic                   -- ^ USCRIPT_OLD_ITALIC                    = 30
    | Oriya                       -- ^ USCRIPT_ORIYA                         = 31
    | Runic                       -- ^ USCRIPT_RUNIC                         = 32
    | Sinhala                     -- ^ USCRIPT_SINHALA                       = 33
    | Syriac                      -- ^ USCRIPT_SYRIAC                        = 34
    | Tamil                       -- ^ USCRIPT_TAMIL                         = 35
    | Telugu                      -- ^ USCRIPT_TELUGU                        = 36
    | Thaana                      -- ^ USCRIPT_THAANA                        = 37
    | Thai                        -- ^ USCRIPT_THAI                          = 38
    | Tibetan                     -- ^ USCRIPT_TIBETAN                       = 39
    | CanadianAboriginal          -- ^ USCRIPT_CANADIAN_ABORIGINAL           = 40
    | Yi                          -- ^ USCRIPT_YI                            = 41
    | Tagalog                     -- ^ USCRIPT_TAGALOG                       = 42
    | Hanunoo                     -- ^ USCRIPT_HANUNOO                       = 43
    | Buhid                       -- ^ USCRIPT_BUHID                         = 44
    | Tagbanwa                    -- ^ USCRIPT_TAGBANWA                      = 45
    | Braille                     -- ^ USCRIPT_BRAILLE                       = 46
    | Cypriot                     -- ^ USCRIPT_CYPRIOT                       = 47
    | Limbu                       -- ^ USCRIPT_LIMBU                         = 48
    | LinearB                     -- ^ USCRIPT_LINEAR_B                      = 49
    | Osmanya                     -- ^ USCRIPT_OSMANYA                       = 50
    | Shavian                     -- ^ USCRIPT_SHAVIAN                       = 51
    | TaiLe                       -- ^ USCRIPT_TAI_LE                        = 52
    | Ugaritic                    -- ^ USCRIPT_UGARITIC                      = 53
    | KatakanaOrHiragana          -- ^ USCRIPT_KATAKANA_OR_HIRAGANA          = 54
    | Buginese                    -- ^ USCRIPT_BUGINESE                      = 55
    | Glagolitic                  -- ^ USCRIPT_GLAGOLITIC                    = 56
    | Kharoshthi                  -- ^ USCRIPT_KHAROSHTHI                    = 57
    | SylotiNagri                 -- ^ USCRIPT_SYLOTI_NAGRI                  = 58
    | NewTaiLue                   -- ^ USCRIPT_NEW_TAI_LUE                   = 59
    | Tifinagh                    -- ^ USCRIPT_TIFINAGH                      = 60
    | OldPersian                  -- ^ USCRIPT_OLD_PERSIAN                   = 61
    | Balinese                    -- ^ USCRIPT_BALINESE                      = 62
    | Batak                       -- ^ USCRIPT_BATAK                         = 63
    | Blissymbols                 -- ^ USCRIPT_BLISSYMBOLS                   = 64
    | Brahmi                      -- ^ USCRIPT_BRAHMI                        = 65
    | Cham                        -- ^ USCRIPT_CHAM                          = 66
    | Cirth                       -- ^ USCRIPT_CIRTH                         = 67
    | Oldchurchslavoniccyrillic   -- ^ USCRIPT_OLD_CHURCH_SLAVONIC_CYRILLIC  = 68
    | DemoticEgyptian             -- ^ USCRIPT_DEMOTIC_EGYPTIAN              = 69
    | HieraticEgyptian            -- ^ USCRIPT_HIERATIC_EGYPTIAN             = 70
    | EgyptianHieroglyphs         -- ^ USCRIPT_EGYPTIAN_HIEROGLYPHS          = 71
    | Khutsuri                    -- ^ USCRIPT_KHUTSURI                      = 72
    | SimplifiedHan               -- ^ USCRIPT_SIMPLIFIED_HAN                = 73
    | TraditionalHan              -- ^ USCRIPT_TRADITIONAL_HAN               = 74
    | PahawhHmong                 -- ^ USCRIPT_PAHAWH_HMONG                  = 75
    | OldHungarian                -- ^ USCRIPT_OLD_HUNGARIAN                 = 76
    | HarappanIndus               -- ^ USCRIPT_HARAPPAN_INDUS                = 77
    | Javanese                    -- ^ USCRIPT_JAVANESE                      = 78
    | KayahLi                     -- ^ USCRIPT_KAYAH_LI                      = 79
    | LatinFraktur                -- ^ USCRIPT_LATIN_FRAKTUR                 = 80
    | LatinGaelic                 -- ^ USCRIPT_LATIN_GAELIC                  = 81
    | Lepcha                      -- ^ USCRIPT_LEPCHA                        = 82
    | LinearA                     -- ^ USCRIPT_LINEAR_A                      = 83
    | Mandaic                     -- ^ USCRIPT_MANDAIC                       = 84
    | MayanHieroglyphs            -- ^ USCRIPT_MAYAN_HIEROGLYPHS             = 85
    | MeroiticHieroglyphs         -- ^ USCRIPT_MEROITIC_HIEROGLYPHS          = 86
    | Nko                         -- ^ USCRIPT_NKO                           = 87
    | Orkhon                      -- ^ USCRIPT_ORKHON                        = 88
    | OldPermic                   -- ^ USCRIPT_OLD_PERMIC                    = 89
    | PhagsPa                     -- ^ USCRIPT_PHAGS_PA                      = 90
    | Phoenician                  -- ^ USCRIPT_PHOENICIAN                    = 91
    | Miao                        -- ^ USCRIPT_MIAO                          = 92
    | Rongorongo                  -- ^ USCRIPT_RONGORONGO                    = 93
    | Sarati                      -- ^ USCRIPT_SARATI                        = 94
    | EstrangeloSyriac            -- ^ USCRIPT_ESTRANGELO_SYRIAC             = 95
    | WesternSyriac               -- ^ USCRIPT_WESTERN_SYRIAC                = 96
    | EasternSyriac               -- ^ USCRIPT_EASTERN_SYRIAC                = 97
    | Tengwar                     -- ^ USCRIPT_TENGWAR                       = 98
    | Vai                         -- ^ USCRIPT_VAI                           = 99
    | VisibleSpeech               -- ^ USCRIPT_VISIBLE_SPEECH                = 100
    | Cuneiform                   -- ^ USCRIPT_CUNEIFORM                     = 101
    | UnwrittenLanguages          -- ^ USCRIPT_UNWRITTEN_LANGUAGES           = 102
    | Unknown                     -- ^ USCRIPT_UNKNOWN                       = 103
    | Carian                      -- ^ USCRIPT_CARIAN                        = 104
    | Japanese                    -- ^ USCRIPT_JAPANESE                      = 105
    | Lanna                       -- ^ USCRIPT_LANNA                         = 106
    | Lycian                      -- ^ USCRIPT_LYCIAN                        = 107
    | Lydian                      -- ^ USCRIPT_LYDIAN                        = 108
    | OlChiki                     -- ^ USCRIPT_OL_CHIKI                      = 109
    | Rejang                      -- ^ USCRIPT_REJANG                        = 110
    | Saurashtra                  -- ^ USCRIPT_SAURASHTRA                    = 111
    | SignWriting                 -- ^ USCRIPT_SIGN_WRITING                  = 112
    | Sundanese                   -- ^ USCRIPT_SUNDANESE                     = 113
    | Moon                        -- ^ USCRIPT_MOON                          = 114
    | MeiteiMayek                 -- ^ USCRIPT_MEITEI_MAYEK                  = 115
    | ImperialAramaic             -- ^ USCRIPT_IMPERIAL_ARAMAIC              = 116
    | Avestan                     -- ^ USCRIPT_AVESTAN                       = 117
    | Chakma                      -- ^ USCRIPT_CHAKMA                        = 118
    | Korean                      -- ^ USCRIPT_KOREAN                        = 119
    | Kaithi                      -- ^ USCRIPT_KAITHI                        = 120
    | Manichaean                  -- ^ USCRIPT_MANICHAEAN                    = 121
    | InscriptionalPahlavi        -- ^ USCRIPT_INSCRIPTIONAL_PAHLAVI         = 122
    | PsalterPahlavi              -- ^ USCRIPT_PSALTER_PAHLAVI               = 123
    | BookPahlavi                 -- ^ USCRIPT_BOOK_PAHLAVI                  = 124
    | InscriptionalParthian       -- ^ USCRIPT_INSCRIPTIONAL_PARTHIAN        = 125
    | Samaritan                   -- ^ USCRIPT_SAMARITAN                     = 126
    | TaiViet                     -- ^ USCRIPT_TAI_VIET                      = 127
    | MathematicalNotation        -- ^ USCRIPT_MATHEMATICAL_NOTATION         = 128
    | Symbols                     -- ^ USCRIPT_SYMBOLS                       = 129
    | Bamum                       -- ^ USCRIPT_BAMUM                         = 130
    | Lisu                        -- ^ USCRIPT_LISU                          = 131
    | NakhiGeba                   -- ^ USCRIPT_NAKHI_GEBA                    = 132
    | OldSouthArabian             -- ^ USCRIPT_OLD_SOUTH_ARABIAN             = 133
    | BassaVah                    -- ^ USCRIPT_BASSA_VAH                     = 134
    | Duployan                    -- ^ USCRIPT_DUPLOYAN                      = 135
    | Elbasan                     -- ^ USCRIPT_ELBASAN                       = 136
    | Grantha                     -- ^ USCRIPT_GRANTHA                       = 137
    | Kpelle                      -- ^ USCRIPT_KPELLE                        = 138
    | Loma                        -- ^ USCRIPT_LOMA                          = 139
    | Mende                       -- ^ USCRIPT_MENDE                         = 140
    | MeroiticCursive             -- ^ USCRIPT_MEROITIC_CURSIVE              = 141
    | OldNorthArabian             -- ^ USCRIPT_OLD_NORTH_ARABIAN             = 142
    | Nabataean                   -- ^ USCRIPT_NABATAEAN                     = 143
    | Palmyrene                   -- ^ USCRIPT_PALMYRENE                     = 144
    | Khudawadi                   -- ^ USCRIPT_KHUDAWADI                     = 145
    | WarangCiti                  -- ^ USCRIPT_WARANG_CITI                   = 146
    | Afaka                       -- ^ USCRIPT_AFAKA                         = 147
    | Jurchen                     -- ^ USCRIPT_JURCHEN                       = 148
    | Mro                         -- ^ USCRIPT_MRO                           = 149
    | Nushu                       -- ^ USCRIPT_NUSHU                         = 150
    | Sharada                     -- ^ USCRIPT_SHARADA                       = 151
    | SoraSompeng                 -- ^ USCRIPT_SORA_SOMPENG                  = 152
    | Takri                       -- ^ USCRIPT_TAKRI                         = 153
    | Tangut                      -- ^ USCRIPT_TANGUT                        = 154
    | Woleai                      -- ^ USCRIPT_WOLEAI                        = 155
    | AnatolianHieroglyphs        -- ^ USCRIPT_ANATOLIAN_HIEROGLYPHS         = 156
    | Khojki                      -- ^ USCRIPT_KHOJKI                        = 157
    | Tirhuta                     -- ^ USCRIPT_TIRHUTA                       = 158
    | CaucasianAlbanian           -- ^ USCRIPT_CAUCASIAN_ALBANIAN            = 159
    | Mahajani                    -- ^ USCRIPT_MAHAJANI                      = 160
    | Ahom                        -- ^ USCRIPT_AHOM                          = 161
    | Hatran                      -- ^ USCRIPT_HATRAN                        = 162
    | Modi                        -- ^ USCRIPT_MODI                          = 163
    | Multani                     -- ^ USCRIPT_MULTANI                       = 164
    | PauCinHau                   -- ^ USCRIPT_PAU_CIN_HAU                   = 165
    | Siddham                     -- ^ USCRIPT_SIDDHAM                       = 166
    | Adlam                       -- ^ USCRIPT_ADLAM                         = 167
    | Bhaiksuki                   -- ^ USCRIPT_BHAIKSUKI                     = 168
    | Marchen                     -- ^ USCRIPT_MARCHEN                       = 169
    | Newa                        -- ^ USCRIPT_NEWA                          = 170
    | Osage                       -- ^ USCRIPT_OSAGE                         = 171
    | HanWithBopomofo             -- ^ USCRIPT_HAN_WITH_BOPOMOFO             = 172
    | Jamo                        -- ^ USCRIPT_JAMO                          = 173
    | SymbolsEmoji                -- ^ USCRIPT_SYMBOLS_EMOJI                 = 174
    | MasaramGondi                -- ^ USCRIPT_MASARAM_GONDI                 = 175
    | Soyombo                     -- ^ USCRIPT_SOYOMBO                       = 176
    | ZanabazarSquare             -- ^ USCRIPT_ZANABAZAR_SQUARE              = 177
    | Dogra                       -- ^ USCRIPT_DOGRA                         = 178
    | GunjalaGondi                -- ^ USCRIPT_GUNJALA_GONDI                 = 179
    | Makasar                     -- ^ USCRIPT_MAKASAR                       = 180
    | Medefaidrin                 -- ^ USCRIPT_MEDEFAIDRIN                   = 181
    | HanifiRohingya              -- ^ USCRIPT_HANIFI_ROHINGYA               = 182
    | Sogdian                     -- ^ USCRIPT_SOGDIAN                       = 183
    | OldSogdian                  -- ^ USCRIPT_OLD_SOGDIAN                   = 184
    | Elymaic                     -- ^ USCRIPT_ELYMAIC                       = 185
    | NyiakengPuachueHmong        -- ^ USCRIPT_NYIAKENG_PUACHUE_HMONG        = 186
    | Nandinagari                 -- ^ USCRIPT_NANDINAGARI                   = 187
    | Wancho                      -- ^ USCRIPT_WANCHO                        = 188
    | Chorasmian                  -- ^ USCRIPT_CHORASMIAN                    = 189
    | DivesAkuru                  -- ^ USCRIPT_DIVES_AKURU                   = 190
    | KhitanSmallScript           -- ^ USCRIPT_KHITAN_SMALL_SCRIPT           = 191
    | Yezidi                      -- ^ USCRIPT_YEZIDI                        = 192
    | CyproMinoan                 -- ^ USCRIPT_CYPRO_MINOAN                  = 193
    | OldUyghur                   -- ^ USCRIPT_OLD_UYGHUR                    = 194
    | Tangsa                      -- ^ USCRIPT_TANGSA                        = 195
    | Toto                        -- ^ USCRIPT_TOTO                          = 196
    | Vithkuqi                    -- ^ USCRIPT_VITHKUQI                      = 197
    | Kawi                        -- ^ USCRIPT_KAWI                          = 198
    | NagMundari                  -- ^ USCRIPT_NAG_MUNDARI                   = 199
    | Aran                        -- ^ USCRIPT_ARABIC_NASTALIQ               = 200
    | Gara                        -- ^ USCRIPT_GARAY                         = 201
    | Gukh                        -- ^ USCRIPT_GURUNG_KHEMA                  = 202
    | Krai                        -- ^ USCRIPT_KIRAT_RAI                     = 203
    | Onao                        -- ^ USCRIPT_OL_ONAL                       = 204
    | Sunu                        -- ^ USCRIPT_SUNUWAR                       = 205
    | Todr                        -- ^ USCRIPT_TODHRI                        = 206
    | Tutg                        -- ^ USCRIPT_TULU_TIGALARI                 = 207
    deriving (Bounded, Enum, Eq, Show)
