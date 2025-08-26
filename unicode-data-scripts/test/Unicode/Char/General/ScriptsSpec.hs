{-# LANGUAGE BlockArguments, OverloadedLists #-}

module Unicode.Char.General.ScriptsSpec
  ( spec
  ) where

import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import GHC.Exts
    ( isTrue#, orI#, andI#, (-#), (<#), (<=#), (==#)
    , Char (..), ord#
    , plusAddr#, eqAddr#, nullAddr#, ltAddr# )
import Test.Hspec
    ( expectationFailure, shouldBe, shouldSatisfy, it, describe
    , Spec, Expectation, HasCallStack )
import Unicode.Internal.Bits.Scripts (nextInt32#)
import qualified Unicode.Char.General.Scripts as S
import qualified Unicode.Internal.Char.Scripts as IS

{- [NOTE]
These tests may fail if the compiler’s Unicode version
does not match the version of this package.

+-------------+----------------+-----------------+
| GHC version | @base@ version | Unicode version |
+=============+================+=================+
| 8.8         | 4.13           | 12.0            |
| 8.10.[1-4]  | 4.14.{0,1}     | 12.0            |
| 8.10.5+     | 4.14.2+        | 13.0            |
| 9.0.[1-2]   | 4.15.0         | 12.1            |
| 9.2.[1-6]   | 4.16.0         | 14.0            |
| 9.4.[1-4]   | 4.17.0         | 14.0            |
| 9.6.1       | 4.18.0         | 15.0            |
+-------------+----------------+-----------------+
-}

spec :: Spec
spec = do
  describe "Unicode scripts" do
    describe "Examples" do
        it "script" do
            let check s = (== s) . S.script
            minBound  `shouldSatisfy` check S.Common
            maxBound  `shouldSatisfy` check S.Unknown
            '.'       `shouldSatisfy` check S.Common
            '1'       `shouldSatisfy` check S.Common
            'A'       `shouldSatisfy` check S.Latin
            'Α'       `shouldSatisfy` check S.Greek -- Greek capital
            'α'       `shouldSatisfy` check S.Greek
            '\x0300'  `shouldSatisfy` check S.Inherited
            '\x0485'  `shouldSatisfy` check S.Inherited
            '\x0600'  `shouldSatisfy` check S.Arabic
            '\x060c'  `shouldSatisfy` check S.Common
            '\x0965'  `shouldSatisfy` check S.Common
            '\x1100'  `shouldSatisfy` check S.Hangul
            '\x3000'  `shouldSatisfy` check S.Common
            '\x4E00'  `shouldSatisfy` check S.Han
            '\x11FD0' `shouldSatisfy` check S.Tamil
            '\x1F600' `shouldSatisfy` check S.Common
            '\x20000' `shouldSatisfy` check S.Han
            -- BOM
            '\xFEFF'  `shouldSatisfy` check S.Common
            '\xFFFF'  `shouldSatisfy` check S.Unknown
            -- Private Use Areas
            '\xE000'  `shouldSatisfy` check S.Unknown
            '\xF0000' `shouldSatisfy` check S.Unknown
        it "scriptExtensions" do
            let shouldBe' :: (HasCallStack) => Char -> NE.NonEmpty S.Script -> Expectation
                shouldBe' c = shouldBe (S.scriptExtensions c)
            minBound  `shouldBe'` [ S.Common]
            maxBound  `shouldBe'` [ S.Unknown]
            '.'       `shouldBe'` [ S.Common]
            '1'       `shouldBe'` [ S.Common]
            'A'       `shouldBe'` [ S.Latin]
            'Α'       `shouldBe'` [ S.Greek]
            'α'       `shouldBe'` [ S.Greek]
            '\x0300'  `shouldBe'` [ S.Cherokee
                                  , S.Coptic
                                  , S.Cyrillic
                                  , S.Greek
                                  , S.Latin
                                  , S.OldPermic
                                  , S.Sunuwar
                                  , S.TaiLe ]
            '\x031F'  `shouldBe'` [ S.Inherited]
            '\x0485'  `shouldBe'` [ S.Cyrillic, S.Latin]
            '\x0600'  `shouldBe'` [ S.Arabic]
            '\x060C'  `shouldBe'` [ S.Arabic
                                  , S.Garay
                                  , S.HanifiRohingya
                                  , S.Nko
                                  , S.Syriac
                                  , S.Thaana
                                  , S.Yezidi ]
            '\x0965'  `shouldBe'` [ S.Bengali
                                  , S.Devanagari
                                  , S.Dogra
                                  , S.Grantha
                                  , S.Gujarati
                                  , S.GunjalaGondi
                                  , S.Gurmukhi
                                  , S.GurungKhema
                                  , S.Kannada
                                  , S.Khudawadi
                                  , S.Limbu
                                  , S.Mahajani
                                  , S.Malayalam
                                  , S.MasaramGondi
                                  , S.Nandinagari
                                  , S.OlOnal
                                  , S.Oriya
                                  , S.Sinhala
                                  , S.SylotiNagri
                                  , S.Takri
                                  , S.Tamil
                                  , S.Telugu
                                  , S.Tirhuta ]
            '\x1100'  `shouldBe'` [ S.Hangul]
            '\x1805'  `shouldBe'` [ S.Mongolian, S.PhagsPa ]
            '\x3001'  `shouldBe'` [ S.Bopomofo
                                  , S.Han
                                  , S.Hangul
                                  , S.Hiragana
                                  , S.Katakana
                                  , S.Mongolian
                                  , S.Yi ]
            '\x4E00'  `shouldBe'` [ S.Han]
            '\x11FD0' `shouldBe'` [ S.Grantha, S.Tamil ]
            '\x1F600' `shouldBe'` [ S.Common]
            '\x20000' `shouldBe'` [ S.Han]
            -- BOM
            '\xFEFF'  `shouldBe'` [ S.Common ]
            '\xFFFF'  `shouldBe'` [ S.Unknown ]
            -- Private Use Areas
            '\xE000'  `shouldBe'` [ S.Unknown ]
            '\xF0000' `shouldBe'` [ S.Unknown ]
        it "scriptDefinition" do
            take 304 (S.scriptDefinition S.Latin) `shouldBe`
                "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzªºÀÁÂÃÄÅÆÇ\
                \ÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿĀāĂăĄąĆć\
                \ĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅ\
                \ņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſƀƁƂƃ\
                \ƄƅƆƇƈƉƊƋƌƍƎƏƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞƟƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯưƱƲƳƴƵƶƷƸƹƺƻ"
            S.scriptDefinition S.ZanabazarSquare `shouldBe`
                ['\x11A00'..'\x11A47']
            S.scriptDefinition S.Lydian `shouldBe`
                (['\x10920'..'\x10939'] <> "\x1093F")
    it "Smallest script definitions have at least one range in their bitmap"
        let {
            check s = case IS.scriptDefinition s of
                (# _, _, addr#, n# #) -> case n# of
                    0# -> isTrue# (addr# `eqAddr#` nullAddr#)
                    4# ->
                        let lower = nextInt32# addr#
                            upper = nextInt32# (addr# `plusAddr#` 4#)
                        -- Check we have a range
                        in isTrue# ((lower <# IS.ScriptCharMask) `andI#`
                                    (upper <# IS.ScriptCharMask) `andI#`
                                    (1# <# (upper -# lower)) )
                    _ -> True
        } in traverse_ (`shouldSatisfy` check) (enumFromTo minBound maxBound)
    it "Characters are in the definition of their corresponding script"
        let {
            check c =
                let s = S.script c
                in if c `inScript` s
                    then pure ()
                    else expectationFailure $ mconcat
                        [ "Char “", show c, "” in not in the definition of “"
                        , show s, "”." ]
        } in traverse_ check (enumFromTo minBound maxBound)
    it "Characters in a script definition have the corresponding script"
        let {
            checkChar s c = let s' = S.script c in if s' == s
                then pure ()
                else expectationFailure $ mconcat
                    [ "Script is different for “", show c, "”. Expected: “"
                    , show s, "” but got: “", show s', "”." ];
            check s = case S.scriptDefinition s of
                        [] -> expectationFailure $ mconcat
                              ["Script “", show s, "” has an empty definition"]
                        chars -> traverse_ (checkChar s) chars
        } in traverse_ check (enumFromTo minBound maxBound)
    it "Characters in with a script extension different from its script"
        let {
            check c =
                let script = S.script c
                    exts = S.scriptExtensions c
                in if  exts == pure script
                    || (isSpecialScript script && script `notElem` exts)
                    || (script `elem` exts)
                    then pure ()
                    else expectationFailure (show (c, script, exts));
            isSpecialScript = \case
                S.Common    -> True
                S.Inherited -> True
                _                  -> False
        } in traverse_ check (enumFromTo minBound maxBound)

-- | Return 'True' if a character is in a script.
-- Faster than checking scriptDefinition directly.
inScript :: Char -> S.Script -> Bool
inScript (C# c#) s = case IS.scriptDefinition s of
    (# lower#, upper#, addr0#, offset# #) ->
        case (cp# <# lower#) `orI#` (upper# <# cp#) of
            1# -> False
            _  -> case offset# of
                0# -> True
                _  -> isTrue# ((cp# ==# lower#) `orI#` (cp# ==# upper#))
                   || check addr0#
        where
        cp# = ord# c#
        addr1# = addr0# `plusAddr#` offset#
        check addr# = case addr1# `ltAddr#` addr# of
            1# -> False
            _  -> case nextInt32# addr# of
                cp1# -> case andI# cp1# IS.ScriptCharMask of
                    -- Range
                    0# ->
                        isTrue# ((cp1# <=# cp#) `andI#`
                                 (cp# <=# nextInt32# (addr# `plusAddr#` 4#)))
                        || check (addr# `plusAddr#` 8#)
                    -- Single char
                    _ -> case andI# IS.ScriptCharMaskComplement cp1# -# cp# of
                        0# -> True
                        _  -> check (addr# `plusAddr#` 4#)
