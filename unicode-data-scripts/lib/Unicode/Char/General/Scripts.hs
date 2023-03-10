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
    ( S.Script(..)
    , script
    , scriptShortName
    , scriptDefinition
    , scriptExtensions )
where

import qualified Data.List.NonEmpty as NE
import GHC.Exts
       ( Char(..), chr#
       , plusAddr#, ltAddr#
       , isTrue#, andI#, tagToEnum# )

import Unicode.Internal.Bits.Scripts (nextInt8#, nextInt32#, unpackCString#)
import qualified Unicode.Internal.Char.Scripts as S
import qualified Unicode.Internal.Char.ScriptExtensions as S

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
    (# firstScript#, secondScript#, addr0#, lastOffset# #) ->
        (tagToEnum# firstScript# :: S.Script) NE.:| case lastOffset# of
            -2# -> []
            -1# -> [tagToEnum# secondScript# :: S.Script]
            0#  -> [tagToEnum# secondScript#, tagToEnum# (nextInt8# addr0#) :: S.Script]
            _   -> (tagToEnum# secondScript# :: S.Script)
                 : (tagToEnum# (nextInt8# addr0#) :: S.Script)
                 : go (addr0# `plusAddr#` 1#)
                where
                addr1# = addr0# `plusAddr#` lastOffset#
                go addr# = case addr1# `ltAddr#` addr# of
                    1# -> []
                    _  -> (tagToEnum# (nextInt8# addr#) :: S.Script)
                        : go (addr# `plusAddr#` 1#)
