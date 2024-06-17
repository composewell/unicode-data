-- |
-- Module      : Unicode.Char.Identifiers.Security
-- Copyright   : (c) 2021 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode Security Mechanisms functions based on
-- [Unicode Technical Standard #39](https://www.unicode.org/reports/tr39/).
--
-- @since 0.1.0

module Unicode.Char.Identifiers.Security
    ( -- * Unicode version
      unicodeVersion
      -- * Identifier status
    , isAllowedInIdentifier

      -- * Identifier type
    , T.IdentifierType(..)
    , identifierTypes
    , isIdentifierTypeAllowed

      -- * Confusables
    , confusablePrototype
    -- , prototype
    , intentionalConfusables
    , isIntentionallyConfusable
    )
where

import Data.List.NonEmpty (NonEmpty)
import GHC.Exts (eqChar#, indexCharOffAddr#, isTrue#)

import Unicode.Internal.Bits.Security (unpackCStringUtf8#)
import qualified Unicode.Internal.Char.Security.Confusables as C
import qualified Unicode.Internal.Char.Security.IdentifierStatus as S
import qualified Unicode.Internal.Char.Security.IdentifierType as T
import qualified Unicode.Internal.Char.Security.IntentionalConfusables as IC
import Unicode.Internal.Char.Security.Version (unicodeVersion)

-- | Returns 'True' if the given character is allowed in an identifier.
--
-- * /Restricted/ characters should be treated with caution when considering
-- possible use in identifiers, and should be disallowed unless there is
-- good reason to allow them in the environment in question.
-- * /Allowed/ characters are not typically used as is by implementations.
-- Instead, they are applied as filters to the set of supported characters.
--
-- @since 0.1.0
{-# INLINE isAllowedInIdentifier #-}
isAllowedInIdentifier :: Char -> Bool
isAllowedInIdentifier = S.isAllowedInIdentifier

-- | Return 'True' if the given 'T.IdentifierType' is allowed.
--
-- @since 0.1.0
{-# INLINE isIdentifierTypeAllowed #-}
isIdentifierTypeAllowed :: T.IdentifierType -> Bool
isIdentifierTypeAllowed = \case
    T.Inclusion   -> True
    T.Recommended -> True
    _             -> False

-- | Returns the 'IdentifierType's corresponding to a character.
--
-- @since 0.1.0
{-# INLINE identifierTypes #-}
identifierTypes :: Char -> NonEmpty T.IdentifierType
identifierTypes = T.decodeIdentifierTypes . T.identifierTypes

-- | Returns the /prototype/ of a character if it is /unintentionally/
-- confusable, else 'Nothing'.
--
-- @since 0.1.0
{-# INLINE confusablePrototype #-}
confusablePrototype :: Char -> String
confusablePrototype c = unpackCStringUtf8# (C.confusablePrototype c)

-- [TODO] Assess the need for this function
-- -- | Returns the /prototype/ of a character.
-- --
-- -- Note: returns the character itself if it is not /unintentionally/ confusable.
-- --
-- -- @since 0.1.0
-- {-# INLINE prototype #-}
-- prototype :: Char -> String
-- prototype c = fromMaybe [c] (confusablePrototype c)

-- | Returns the list of /intentional/ confusables of a character, if any.
--
-- @since 0.1.0
{-# INLINE intentionalConfusables #-}
intentionalConfusables :: Char -> String
intentionalConfusables c = unpackCStringUtf8# (IC.intentionalConfusables c)

-- | Returns 'True' if the character is /intentionally/ confusable.
--
-- @since 0.1.0
{-# INLINE isIntentionallyConfusable #-}
isIntentionallyConfusable :: Char -> Bool
isIntentionallyConfusable c = isTrue# (c# `eqChar#` '\0'#)
    where
    !addr = IC.intentionalConfusables c
    !c# = indexCharOffAddr# addr 0#
