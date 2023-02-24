-- |
-- Module      : Unicode.Char.General.Names
-- Copyright   : (c) 2022 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode character names and name aliases.
-- See Unicode standard 15.0.0, section 4.8.
--
-- @since 0.1.0

module Unicode.Char.General.Names
    ( -- * Name
      name
    , nameOrAlias
    , correctedName
      -- * Name Aliases
    , NameAliases.NameAliasType(..)
    , nameAliases
    , nameAliasesByType
    , nameAliasesWithTypes
    ) where

import Control.Applicative ((<|>))
import GHC.Exts
    ( Addr#, Char(..), Int#, Int(..)
    , indexCharOffAddr#, plusAddr#, (+#), (-#), (<#), isTrue#, quotRemInt#
    , dataToTag#, ord#, Char# )

import Unicode.Internal.Bits.Names (SPEC(..), unpackCString#)
import qualified Unicode.Internal.Char.UnicodeData.DerivedName as DerivedName
import qualified Unicode.Internal.Char.UnicodeData.NameAliases as NameAliases

-- | Name of a character, if defined.
--
-- @since 0.1.0
name :: Char -> Maybe String
name (C# c#) = case DerivedName.name c# of
    (# name#, len# #) -> case len# of
        DerivedName.NoName -> Nothing
        DerivedName.CjkCompatibilityIdeograph -> Just n
            where
            !hex = showHex c#
            !n = 'C':'J':'K':' ':'C':'O':'M':'P':'A':'T':'I':'B':'I':'L':'I':'T':'Y':' ':'I':'D':'E':'O':'G':'R':'A':'P':'H':'-':hex
        DerivedName.CjkUnifiedIdeograph -> Just n
            where
            !hex = showHex c#
            !n = 'C':'J':'K':' ':'U':'N':'I':'F':'I':'E':'D':' ':'I':'D':'E':'O':'G':'R':'A':'P':'H':'-':hex
        DerivedName.TangutIdeograph -> Just n
            where
            !hex = showHex c#
            !n = 'T':'A':'N':'G':'U':'T':' ':'I':'D':'E':'O':'G':'R':'A':'P':'H':'-':hex
        _
            | isTrue# (len# <# DerivedName.HangulSyllable) -> let !n = unpack name# [] len# in Just n
            | otherwise ->
                let !rest = unpack name# [] (len# -# DerivedName.HangulSyllable)
                    !n = 'H':'A':'N':'G':'U':'L':' ':'S':'Y':'L':'L':'A':'B':'L':'E':' ':rest
                in Just n

{-# INLINE unpack #-}
unpack :: Addr# -> String -> Int# -> String
unpack !addr# !acc = \case
    0# -> acc
    !i -> unpack addr# acc' i'
        where
        !i' = i -# 1#
        !c'# = indexCharOffAddr# addr# i'
        !c' = C# c'#
        !acc' = c' : acc

-- [NOTE] We assume c# >= '\x1000' to avoid to check for padding
showHex :: Char# -> String
showHex !c# = showIt [] (quotRemInt# (ord# c#) 16#)
    where
    showIt !acc (# q, r #) = case q of
        0# -> acc'
        _  -> showIt acc' (quotRemInt# q 16#)
        where
        !c = case r of
            0#  -> '0'
            1#  -> '1'
            2#  -> '2'
            3#  -> '3'
            4#  -> '4'
            5#  -> '5'
            6#  -> '6'
            7#  -> '7'
            8#  -> '8'
            9#  -> '9'
            10# -> 'A'
            11# -> 'B'
            12# -> 'C'
            13# -> 'D'
            14# -> 'E'
            _   -> 'F'
        !acc' = c : acc

-- | Returns /corrected/ name of a character (see 'NameAliases.Correction'),
-- if defined, otherwise returns its original 'name' if defined.
--
-- @since 0.1.0
{-# INLINE correctedName #-}
correctedName :: Char -> Maybe String
correctedName c@(C# c#) = corrected <|> name c
    where
    -- Assumption: fromEnum NameAliases.Correction == 0
    !corrected = case indexCharOffAddr# addr# 0# of
        '\xff'# -> Nothing -- no aliases
        '\x00'# -> Nothing -- no correction
        i#      ->
            let !n = unpackCString# (addr# `plusAddr#` (ord# i# +# 1#))
            in Just n
    !addr# = NameAliases.nameAliases c#

-- | Returns a character’s 'name' if defined,
-- otherwise returns its first name alias if defined.
--
-- @since 0.1.0
nameOrAlias :: Char -> Maybe String
nameOrAlias c@(C# c#) = name c <|> case indexCharOffAddr# addr# 0# of
    '\xff'# -> Nothing -- no aliases
    '\x00'# -> let !n = unpackCString# (go 1#) in Just n
    _       -> let !n = unpackCString# (go 0#) in Just n
    where
    !addr# = NameAliases.nameAliases c#
    !(I# maxNameAliasType#) = NameAliases.maxNameAliasType
    go t# = case indexCharOffAddr# (addr# `plusAddr#` t#) 0# of
        '\0'# -> if isTrue# (t# <# maxNameAliasType#)
            then go (t# +# 1#)
            else "\0"# -- impossible: there is at least one alias
        i# -> addr# `plusAddr#` (ord# i# +# 1#)

-- | All name aliases of a character, if defined.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliasesWithTypes' for the detailed list by alias type.
--
-- @since 0.1.0
{-# INLINE nameAliases #-}
nameAliases :: Char -> [String]
nameAliases (C# c#) = case indexCharOffAddr# addr# 0# of
    '\xff'# -> [] -- no aliases
    _       -> foldMap (nameAliasesByType# addr#) [minBound..maxBound]
    where
    addr# = NameAliases.nameAliases c#

-- | Name aliases of a character for a specific name alias type.
--
-- @since 0.1.0
{-# INLINE nameAliasesByType #-}
nameAliasesByType :: NameAliases.NameAliasType -> Char -> [String]
nameAliasesByType t (C# c#) = case indexCharOffAddr# addr# 0# of
    '\xff'# -> [] -- no aliases
    _       -> nameAliasesByType# addr# t
    where
    addr# = NameAliases.nameAliases c#

-- | Detailed character names aliases.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliases' if the alias type is not required.
--
-- @since 0.1.0
{-# INLINE nameAliasesWithTypes #-}
nameAliasesWithTypes :: Char -> [(NameAliases.NameAliasType, [String])]
nameAliasesWithTypes (C# c#) = case indexCharOffAddr# addr# 0# of
    '\xff'# -> [] -- no aliases
    '\x00'# -> foldr mk mempty [succ minBound..maxBound]
    _       -> foldr mk mempty [minBound..maxBound]
    where
    addr# = NameAliases.nameAliases c#
    mk t acc = case nameAliasesByType# addr# t of
        [] -> acc
        as -> (t, as) : acc

{-# INLINE nameAliasesByType# #-}
nameAliasesByType# :: Addr# -> NameAliases.NameAliasType -> [String]
nameAliasesByType# addr# t = case indexCharOffAddr# (addr# `plusAddr#` t#) 0# of
    '\0'# -> [] -- no aliases for this type
    i#    -> unpackCStrings addr# (ord# i#)
    where t# = dataToTag# t

{-# INLINE unpackCStrings #-}
unpackCStrings :: Addr# -> Int# -> [String]
unpackCStrings addr# = go SPEC
    where
    go !_ i# =
        let !s = unpackCString# (addr# `plusAddr#` (i# +# 1#))
        in s : case indexCharOffAddr# (addr# `plusAddr#` i#) 0# of
            '\0'# -> []
            j#    -> go SPEC (ord# j#)


