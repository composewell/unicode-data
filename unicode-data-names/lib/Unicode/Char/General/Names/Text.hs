-- |
-- Module      : Unicode.Char.General.Names.Text
-- Copyright   : (c) 2022 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode character names and name aliases with 'Data.Text' API.
-- See Unicode standard 15.0.0, section 4.8.
--
-- This API is /optional/ and requires using the package flag @has-text@.
--
-- @since 0.3.0

module Unicode.Char.General.Names.Text
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
import qualified Control.Monad.ST as ST
import qualified Data.Text as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Array as A
import GHC.Exts
    ( Addr#, Ptr(..), indexCharOffAddr#, indexWord8OffAddr#, plusAddr#
    , Char(..), ord#
    , Int#, Int(..), (+#), (-#), (<#), isTrue#, quotRemInt#, dataToTag# )
import GHC.Word (Word8(..))

import qualified Unicode.Internal.Char.UnicodeData.DerivedName as DerivedName
import qualified Unicode.Internal.Char.UnicodeData.NameAliases as NameAliases

-- | Name of a character, if defined.
--
-- @since 0.3.0
name :: Char -> Maybe T.Text
name (C# c#) = case DerivedName.name c# of
    (# name#, len# #) -> case len# of
        DerivedName.NoName -> Nothing
        DerivedName.CjkCompatibilityIdeograph -> Just n
            where
            !n = mkNameFromTemplate "CJK COMPATIBILITY IDEOGRAPH-"# 28# (ord# c#)
        DerivedName.CjkUnifiedIdeograph -> Just n
            where
            !n = mkNameFromTemplate "CJK UNIFIED IDEOGRAPH-"# 22# (ord# c#)
        DerivedName.TangutIdeograph -> Just n
            where
            !n = mkNameFromTemplate "TANGUT IDEOGRAPH-"# 17# (ord# c#)
        _
            | isTrue# (len# <# DerivedName.HangulSyllable) ->
                let !n = unpackAddr# name# len#
                in Just n
            | otherwise ->
                let !len = I# (len# -# DerivedName.HangulSyllable +# 16#)
                    ba = ST.runST (do
                        marr <- A.new len
                        A.copyFromPointer marr 0 (Ptr "HANGUL SYLLABLE "#) 16
                        A.copyFromPointer marr 16
                            (Ptr name#)
                            (I# (len# -# DerivedName.HangulSyllable))
                        A.unsafeFreeze marr)
                    !n = T.Text ba 0 len
                in Just n

-- See: unpackCStringAscii#. Here we know the length.
unpackAddr# :: Addr# -> Int# -> T.Text
unpackAddr# addr# len# = T.Text ba 0 (I# len#)
    where
    ba = ST.runST (do
        marr <- A.new (I# len#)
        A.copyFromPointer marr 0 (Ptr addr#) (I# len#)
        A.unsafeFreeze marr)

mkNameFromTemplate :: Addr# -> Int# -> Int# -> T.Text
mkNameFromTemplate template# len# cp# = T.Text ba 0 (I# len'#)
    where
    len'# = len# +# if isTrue# (cp# <# 0x10000#) then 4# else 5#
    ba = ST.runST (do
        marr <- A.new (I# len'#)
        A.copyFromPointer marr 0 (Ptr template#) (I# len#)
        writeHex cp# (len'# -# 1#) marr
        A.unsafeFreeze marr)

-- [NOTE] We assume c# >= '\x1000' to avoid to check for padding
{-# INLINE writeHex #-}
writeHex :: Int# -> Int# -> A.MArray s -> ST.ST s ()
writeHex cp# offset0# !marr = showIt offset0# (quotRemInt# cp# 16#)
    where
    showIt offset# (# q, r #)
        = A.unsafeWrite marr (I# offset#)
                             (W8# (indexWord8OffAddr# "0123456789ABCDEF"# r))
        *> case q of
            0# -> pure ()
            _  -> showIt (offset# -# 1#) (quotRemInt# q 16#)

-- | Returns /corrected/ name of a character (see 'NameAliases.Correction'),
-- if defined, otherwise returns its original 'name' if defined.
--
-- @since 0.3.0
{-# INLINE correctedName #-}
correctedName :: Char -> Maybe T.Text
correctedName c@(C# c#) = corrected <|> name c
    where
    -- Assumption: fromEnum NameAliases.Correction == 0
    !corrected = case ord# (indexCharOffAddr# addr# 0#) of
        0xff# -> Nothing -- no aliases
        0x00# -> Nothing -- no correction
        i#    ->
            let l# = ord# (indexCharOffAddr# (addr# `plusAddr#` i#) 0#)
                !n = unpackAddr# (addr# `plusAddr#` (i# +# 1#)) l#
            in Just n
    !addr# = NameAliases.nameAliases c#

-- | Returns a character’s 'name' if defined,
-- otherwise returns its first name alias if defined.
--
-- @since 0.3.0
nameOrAlias :: Char -> Maybe T.Text
nameOrAlias c@(C# c#) = name c <|> case indexCharOffAddr# addr# 0# of
    '\xff'# -> Nothing -- no aliases
    '\x00'# -> let !n = go 1# in Just n
    _       -> let !n = go 0# in Just n
    where
    !addr# = NameAliases.nameAliases c#
    go t# = case ord# (indexCharOffAddr# (addr# `plusAddr#` t#) 0#) of
        -- No bound check for t#: there is at least one alias
        0# -> go (t# +# 1#)
        i# ->
            let l# = ord# (indexCharOffAddr# (addr# `plusAddr#` i#) 0#)
            in unpackAddr# (addr# `plusAddr#` (i# +# 1#)) l#

-- | All name aliases of a character, if defined.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliasesWithTypes' for the detailed list by alias type.
--
-- @since 0.3.0
{-# INLINE nameAliases #-}
nameAliases :: Char -> [T.Text]
nameAliases (C# c#) = case indexCharOffAddr# addr0# 0# of
    '\xff'# -> [] -- no aliases
    _       -> go (addr0# `plusAddr#` (NameAliases.MaxNameAliasType +# 1#))
        where
        go addr# = case ord# (indexCharOffAddr# addr# 0#) of
            0# -> case ord# (indexCharOffAddr# (addr# `plusAddr#` 1#) 0#) of
                -- End of list
                0# -> []
                -- Skip empty entry
                l# ->
                    let !s = unpackAddr# (addr# `plusAddr#` 2#) l#
                    in s : go (addr# `plusAddr#` (l# +# 2#))
            l# ->
                let !s = unpackAddr# (addr# `plusAddr#` 1#) l#
                in s : go (addr# `plusAddr#` (l# +# 1#))
    where
    addr0# = NameAliases.nameAliases c#

-- | Name aliases of a character for a specific name alias type.
--
-- @since 0.3.0
{-# INLINE nameAliasesByType #-}
nameAliasesByType :: NameAliases.NameAliasType -> Char -> [T.Text]
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
-- @since 0.3.0
{-# INLINE nameAliasesWithTypes #-}
nameAliasesWithTypes :: Char -> [(NameAliases.NameAliasType, [T.Text])]
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
nameAliasesByType# :: Addr# -> NameAliases.NameAliasType -> [T.Text]
nameAliasesByType# addr0# t =
    case ord# (indexCharOffAddr# (addr0# `plusAddr#` dataToTag# t) 0#) of
        0# -> [] -- no aliases for this type
        i# -> go (addr0# `plusAddr#` i#)
            where
            go addr# = case ord# (indexCharOffAddr# addr# 0#) of
                0# -> []
                l# ->
                    let !s = unpackAddr# (addr# `plusAddr#` 1#) l#
                    in s : go (addr# `plusAddr#` (l# +# 1#))
