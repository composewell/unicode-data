-- |
-- Module      : Unicode.Char.General.Names
-- Copyright   : (c) 2022 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode character names and name aliases with 'String' API.
-- See Unicode standard 15.0.0, section 4.8.
--
-- There are also two optional APIs:
--
-- * @ByteString@ API, which requires using the package flag @has-bytestring@.
-- * @Text@ API, which requires using the package flag @has-text@.
--
-- @since 0.1.0

module Unicode.Char.General.Names
    ( -- Unicode version
      unicodeVersion
      -- * Name
    , name
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
    ( Addr#, Char(..), Char#, Int#
    , indexCharOffAddr#, plusAddr#, (+#), (-#), (<#), isTrue#, quotRemInt#
    , dataToTag#, ord# )

import Unicode.Internal.Bits.Names (unpackNBytes#)
import qualified Unicode.Internal.Char.UnicodeData.DerivedName as DerivedName
import qualified Unicode.Internal.Char.UnicodeData.NameAliases as NameAliases
import Unicode.Internal.Char.Names.Version (unicodeVersion)

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
        !c = C# (indexCharOffAddr# "0123456789ABCDEF"# r)
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
            let !n = unpackNBytes'# (addr# `plusAddr#` ord# i#)
            in Just n
    !addr# = NameAliases.nameAliases c#

-- | Returns a character’s 'name' if defined,
-- otherwise returns its first name alias if defined.
--
-- @since 0.1.0
nameOrAlias :: Char -> Maybe String
nameOrAlias c@(C# c#) = name c <|> case indexCharOffAddr# addr# 0# of
    '\xff'# -> Nothing -- no aliases
    '\x00'# -> let !n = go 1# in Just n
    _       -> let !n = go 0# in Just n
    where
    !addr# = NameAliases.nameAliases c#
    go t# = case ord# (indexCharOffAddr# (addr# `plusAddr#` t#) 0#) of
        -- No bound check for t#: there is at least one alias
        0# -> go (t# +# 1#)
        i# -> unpackNBytes'# (addr# `plusAddr#` i#)

-- | All name aliases of a character, if defined.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliasesWithTypes' for the detailed list by alias type.
--
-- @since 0.1.0
{-# INLINE nameAliases #-}
nameAliases :: Char -> [String]
nameAliases (C# c#) = case indexCharOffAddr# addr0# 0# of
    '\xff'# -> [] -- no aliases
    _       -> go (addr0# `plusAddr#` (NameAliases.MaxNameAliasType +# 1#))
        where
        go addr# = case ord# (indexCharOffAddr# addr# 0#) of
            0# -> case ord# (indexCharOffAddr# (addr# `plusAddr#` 1#) 0#) of
                -- End of list
                0# -> []
                -- skip empty entry
                l# ->
                    let !s = unpackNBytes# (addr# `plusAddr#` 2#) l#
                    in s : go (addr# `plusAddr#` (l# +# 2#))
            l# ->
                let !s = unpackNBytes# (addr# `plusAddr#` 1#) l#
                in s : go (addr# `plusAddr#` (l# +# 1#))
    where
    addr0# = NameAliases.nameAliases c#

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
        []  -> acc
        !as -> (t, as) : acc

{-# INLINE unpackNBytes'# #-}
unpackNBytes'# :: Addr# -> String
unpackNBytes'# addr# = unpackNBytes#
    (addr# `plusAddr#` 1#)
    (ord# (indexCharOffAddr# addr# 0#))

{-# INLINE nameAliasesByType# #-}
nameAliasesByType# :: Addr# -> NameAliases.NameAliasType -> [String]
nameAliasesByType# addr# t = case indexCharOffAddr# (addr# `plusAddr#` t#) 0# of
    '\0'# -> [] -- no aliases for this type
    i#    -> unpackCStrings# (addr# `plusAddr#` ord# i#)
    where t# = dataToTag# t

{-# INLINE unpackCStrings# #-}
unpackCStrings# :: Addr# -> [String]
unpackCStrings# = go
    where
    go addr# = case ord# (indexCharOffAddr# addr# 0#) of
        0# -> []
        l# ->
            let !s = unpackNBytes# (addr# `plusAddr#` 1#) l#
            in s : go (addr# `plusAddr#` (l# +# 1#))
