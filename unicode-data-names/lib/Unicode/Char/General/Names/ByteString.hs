-- |
-- Module      : Unicode.Char.General.Names.Text
-- Copyright   : (c) 2022 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode character names and name aliases with 'Data.ByteString' API.
-- See Unicode standard 15.0.0, section 4.8.
--
-- This API is /optional/ and requires using the package flag @has-bytestring@.
--
-- @since 0.3.0

module Unicode.Char.General.Names.ByteString
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import GHC.Exts
    ( Addr#, indexCharOffAddr#, indexWord8OffAddr#, plusAddr#
    , Char(..), ord#
    , Int#, Int(..), (+#), (-#), (<#), isTrue#, andI#, uncheckedIShiftRL#
    , dataToTag#
    , newPinnedByteArray#, byteArrayContents#
    , copyAddrToByteArray#, writeWord8Array#
    , unsafeCoerce# )
import GHC.IO (IO(..))
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents (PlainPtr))
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Unicode.Internal.Char.UnicodeData.DerivedName as DerivedName
import qualified Unicode.Internal.Char.UnicodeData.NameAliases as NameAliases

--------------------------------------------------------------------------------
-- Name
--------------------------------------------------------------------------------

-- | Name of a character, if defined.
--
-- @since 0.3.0
name :: Char -> Maybe BS.ByteString
name (C# c#) = case DerivedName.name c# of
    (# name#, len# #) -> case len# of
        DerivedName.NoName -> Nothing
        DerivedName.CjkCompatibilityIdeograph -> Just n
            where
            !n = mkNameFromTemplate# "CJK COMPATIBILITY IDEOGRAPH-"# 28# (ord# c#)
        DerivedName.CjkUnifiedIdeograph -> Just n
            where
            !n = mkNameFromTemplate# "CJK UNIFIED IDEOGRAPH-"# 22# (ord# c#)
        DerivedName.TangutIdeograph -> Just n
            where
            !n = mkNameFromTemplate# "TANGUT IDEOGRAPH-"# 17# (ord# c#)
        _
            | isTrue# (len# <# DerivedName.HangulSyllable) ->
                let !n = unpackAddr# name# len#
                in Just n
            | otherwise ->
                let !n = unsafePack2LenLiteral#
                            16#
                            "HANGUL SYLLABLE "#
                            (len# -# DerivedName.HangulSyllable)
                            name#
                in Just n

{-# INLINE unpackAddr# #-}
unpackAddr# :: Addr# -> Int# -> BS.ByteString
unpackAddr# addr# len# = BS.unsafePackLenLiteral (I# len#) addr#

-- [NOTE] bytestring does not expose enough primitives for working with raw
-- Addr# literals. Therefore we go low level, which performs better than FFI.
-- Inspiration:
-- • Data.ByteString.Internal.unsafeCreate
-- • GHC.ForeignPtr.mallocPlainForeignPtrBytes

-- | Pack 2 'Addr#' literal into a 'BS.ByteString'.
{-# INLINE unsafePack2LenLiteral# #-}
unsafePack2LenLiteral# :: Int# -> Addr# -> Int# -> Addr# -> BS.ByteString
unsafePack2LenLiteral# l1# addr1# l2# addr2# = unsafeDupablePerformIO (
    IO (\s0 -> let l# = l1# +# l2# in case newPinnedByteArray# l# s0 of
        (# s1, marr# #) -> case copyAddrToByteArray# addr1# marr# 0# l1# s1 of
            s2 -> case copyAddrToByteArray# addr2# marr# l1# l2# s2 of
                s3 ->
                    (# s3
                    -- Note: for compatibility reason we cannot use
                    -- mutableByteArrayContents# here.
                    , BS.BS (ForeignPtr (byteArrayContents# (unsafeCoerce# marr#))
                                        (PlainPtr marr#))
                            (I# l#) #)))

-- | Create a 'BS.ByteString' from a template prefix and then format the given
-- codepoint into hexadecimal string.
mkNameFromTemplate# :: Addr# -> Int# -> Int# -> BS.ByteString
mkNameFromTemplate# template# len# cp# =
    let len'# = len# +# if isTrue# (cp# <# 0x10000#) then 4# else 5#
    in unsafeDupablePerformIO (IO (\s0 -> case newPinnedByteArray# len'# s0 of
        (# s1, marr# #) -> case copyAddrToByteArray# template# marr# 0# len# s1 of
            s2 -> case go marr# s2 (len'# -# 1#) cp# of
                s3 ->
                    (# s3
                    , BS.BS (ForeignPtr (byteArrayContents# (unsafeCoerce# marr#))
                                        (PlainPtr marr#))
                            (I# len'#) #)
                where
                -- Write hex in reverse order
                -- [NOTE] We assume cp# >= 0x1000 to avoid to check for padding
                go ma !s !offset = \case
                    0# -> s
                    n  -> go ma
                        (writeWord8Array# ma offset
                            (indexWord8OffAddr# "0123456789ABCDEF"#
                                                (andI# 0xf# n)) s)
                        (offset -# 1#)
                        (uncheckedIShiftRL# n 4#)

    ))

-- | Returns /corrected/ name of a character (see 'NameAliases.Correction'),
-- if defined, otherwise returns its original 'name' if defined.
--
-- @since 0.3.0
{-# INLINE correctedName #-}
correctedName :: Char -> Maybe BS.ByteString
correctedName c@(C# c#) = corrected <|> name c
    where
    -- Assumption: fromEnum NameAliases.Correction == 0
    !corrected = case indexCharOffAddr# addr# 0# of
        '\xff'# -> Nothing -- no aliases
        '\x00'# -> Nothing -- no correction
        i#      ->
            let !n = unpackAddr#
                        (addr# `plusAddr#` (ord# i# +# 1#))
                        (ord# (indexCharOffAddr# (addr# `plusAddr#` ord# i#) 0#))
            in Just n
    !addr# = NameAliases.nameAliases c#

-- | Returns a character’s 'name' if defined,
-- otherwise returns its first name alias if defined.
--
-- @since 0.3.0
nameOrAlias :: Char -> Maybe BS.ByteString
nameOrAlias c@(C# c#) = name c <|> case indexCharOffAddr# addr# 0# of
    '\xff'# -> Nothing -- no aliases
    '\x00'# -> let !n = go 1# in Just n
    _       -> let !n = go 0# in Just n
    where
    !addr# = NameAliases.nameAliases c#
    go t# = case ord# (indexCharOffAddr# (addr# `plusAddr#` t#) 0#) of
        -- No bound check for t#: there is at least one alias
        0# -> go (t# +# 1#)
        i# -> unpackAddr#
                (addr# `plusAddr#` (i# +# 1#))
                (ord# (indexCharOffAddr# (addr# `plusAddr#` i#) 0#))

-- | All name aliases of a character, if defined.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliasesWithTypes' for the detailed list by alias type.
--
-- @since 0.3.0
{-# INLINE nameAliases #-}
nameAliases :: Char -> [BS.ByteString]
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
nameAliasesByType :: NameAliases.NameAliasType -> Char -> [BS.ByteString]
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
nameAliasesWithTypes :: Char -> [(NameAliases.NameAliasType, [BS.ByteString])]
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
nameAliasesByType# :: Addr# -> NameAliases.NameAliasType -> [BS.ByteString]
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
