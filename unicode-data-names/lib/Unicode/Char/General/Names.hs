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

      -- * Label
    , label
    ) where

import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe)
import Foreign.C.String (CString, peekCAString)
import System.IO.Unsafe (unsafePerformIO)

import Unicode.Char (showHexCodePoint)
import Unicode.Char.General (CodePointType(..), codePointType)
import qualified Unicode.Internal.Char.UnicodeData.DerivedName as DerivedName
import qualified Unicode.Internal.Char.UnicodeData.NameAliases as NameAliases

-- | Name of a character, if defined.
--
-- @since 0.1.0
{-# INLINE name #-}
name :: Char -> Maybe String
name = fmap unpack . DerivedName.name

-- | Returns /corrected/ name of a character (see 'NameAliases.Correction'),
-- if defined, otherwise returns its original 'name' if defined.
--
-- @since 0.1.0
{-# INLINE correctedName #-}
correctedName :: Char -> Maybe String
correctedName c =
    listToMaybe (nameAliasesByType NameAliases.Correction c) <|> name c

-- | Returns a character’s 'name' if defined,
-- otherwise returns its first name alias if defined.
--
-- @since 0.1.0
nameOrAlias :: Char -> Maybe String
nameOrAlias c = name c <|> case NameAliases.nameAliasesWithTypes c of
    (_, n:_):_ -> Just (unpack n)
    _          -> Nothing

-- | All name aliases of a character, if defined.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliasesWithTypes' for the detailed list by alias type.
--
-- @since 0.1.0
{-# INLINE nameAliases #-}
nameAliases :: Char -> [String]
nameAliases = fmap unpack . NameAliases.nameAliases

-- | Name aliases of a character for a specific name alias type.
--
-- @since 0.1.0
{-# INLINE nameAliasesByType #-}
nameAliasesByType :: NameAliases.NameAliasType -> Char -> [String]
nameAliasesByType t = fmap unpack . NameAliases.nameAliasesByType t

-- | Detailed character names aliases.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliases' if the alias type is not required.
--
-- @since 0.1.0
{-# INLINE nameAliasesWithTypes #-}
nameAliasesWithTypes :: Char -> [(NameAliases.NameAliasType, [String])]
nameAliasesWithTypes
  = fmap (fmap (fmap unpack))
  . NameAliases.nameAliasesWithTypes

-- | Returns the label of a code point if it has no character name, otherwise
-- returns @\"UNDEFINED\"@.
--
-- See subsection
-- [“Code Point Labels”](https://www.unicode.org/versions/Unicode15.0.0/ch04.pdf#G135248)
-- in section 4.8 “Name” of the Unicode Standard.
--
-- @since 0.2.1
label :: Char -> String
label c = case codePointType c of
    ControlType      -> "control-"      <> cpStr
    PrivateUseType   -> "private-use-"  <> cpStr
    SurrogateType    -> "surrogate-"    <> cpStr
    NoncharacterType -> "noncharacter-" <> cpStr
    ReservedType     -> "reserved-"     <> cpStr
    _                -> "UNDEFINED"

    where

    cpStr = showHexCodePoint c ""

-- Note: names are ASCII. See Unicode Standard 15.0.0, section 4.8.
{-# INLINE unpack #-}
unpack :: CString -> String
unpack = unsafePerformIO . peekCAString
