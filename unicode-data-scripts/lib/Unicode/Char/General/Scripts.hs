-- |
-- Module      : Unicode.Char.General.Scripts
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode [scripts](https://www.unicode.org/reports/tr24/) related functions.
--
-- @since 0.1.0
--

module Unicode.Char.General.Scripts
    ( -- * Unicode version
      unicodeVersion
      -- * Scripts
    , S.Script(..)
    , script
    , scriptShortName
    , scriptDefinition
    , scriptExtensions
    )
where

import qualified Data.List.NonEmpty as NE
import GHC.Exts (
    Addr#,
    Char (..),
    Int#,
    andI#,
    chr#,
    isTrue#,
    ltAddr#,
    negateInt#,
    plusAddr#,
    tagToEnum#,
    (-#),
    (<=#),
 )

import Unicode.Internal.Bits.Scripts (nextInt32#, nextInt8#, unpackCString#)
import qualified Unicode.Internal.Char.ScriptExtensions as S
import qualified Unicode.Internal.Char.Scripts as S
import Unicode.Internal.Char.Scripts.Version (unicodeVersion)

-- | Character [script](https://www.unicode.org/reports/tr24/).
--
-- @since 0.1.0
{-# INLINE script #-}
script :: Char -> S.Script
script c = tagToEnum# (S.script c)

-- | Returns the 4-letter ISO 15924 code of a 'Script'.
--
-- @since 0.3.0
{-# INLINE scriptShortName #-}
scriptShortName :: S.Script -> String
scriptShortName s = unpackCString# (S.scriptShortName s)

{- HLINT ignore scriptDefinition "Eta reduce" -}
-- | Characters corresponding to a 'S.Script'.
--
-- @since 0.1.0
scriptDefinition :: S.Script -> String
scriptDefinition s = case S.scriptDefinition s of
    (# lower#, upper#, addr0#, offset# #) -> case offset# of
        0#  -> [C# (chr# lower#)..C# (chr# upper#)]
        _   -> let {
            addr1# = addr0# `plusAddr#` offset#;
            unpack addr#
                | isTrue# (addr1# `ltAddr#` addr#) = [C# (chr# upper#)]
                | otherwise =
                    let cp1# = nextInt32# addr#
                    in case cp1# `andI#` S.ScriptCharMask of
                        -- Range
                        0# -> foldr (:)
                                (unpack (addr# `plusAddr#` 8#))
                                [ C# (chr# cp1#)
                                ..C# (chr# (nextInt32# (addr# `plusAddr#` 4#)))]
                        -- Char
                        _ -> C# (chr# (andI# S.ScriptCharMaskComplement cp1#))
                           : unpack (addr# `plusAddr#` 4#)
        } in C# (chr# lower#) : unpack addr0#

-- | Character
-- [script extensions](https://www.unicode.org/reports/tr24/#Script_Extensions).
--
-- @since 0.1.0
scriptExtensions :: Char -> NE.NonEmpty S.Script
scriptExtensions c = case S.scriptExtensions c of
    (# n, scripts0 #)
        | isTrue# (n <=# 0#) -> tagToScript (negateInt# n) NE.:| []
        | otherwise          -> tagToScript (nextInt8# scripts0)
                          NE.:| go (scripts0 `plusAddr#` 1#) (n -# 1#)
  where
    tagToScript s = tagToEnum# s :: S.Script
    go :: Addr# -> Int# -> [S.Script]
    go scripts = \case
        0# -> []
        k# -> tagToScript (nextInt8# scripts)
            : go (scripts `plusAddr#` 1#) (k# -# 1#)
