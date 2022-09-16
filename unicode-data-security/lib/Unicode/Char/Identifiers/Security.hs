-- [TODO] @since
-- |
-- Module      : Unicode.Char.Identifiers
-- Copyright   : (c) 2021 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode Security Mechanisms functions based on
-- [Unicode Technical Standard #39](https://www.unicode.org/reports/tr39/)

module Unicode.Char.Identifiers.Security
    ( -- * Identifier status
      IdentifierStatus(..)
    , identifierStatus
    , isAllowedIdentifier

      -- * Identifier type
    , T.IdentifierType(..)
    , identifierTypes
    , identifierTypeStatus

      -- * Confusables
    , prototype
    , prototypeM
    , intentionalConfusables
    , isIntentionallyConfusable
    )
where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (fromMaybe, isJust)
import qualified GHC.Foreign as Foreign
import qualified GHC.IO.Encoding as Encoding
import           System.IO.Unsafe (unsafePerformIO)

import qualified Unicode.Internal.Char.Security.Confusables as C
import qualified Unicode.Internal.Char.Security.IdentifierStatus as S
import qualified Unicode.Internal.Char.Security.IdentifierType as T
import qualified Unicode.Internal.Char.Security.IntentionalConfusables as IC

-- [TODO] @since
-- | Identifier status
--
data IdentifierStatus
    = Restricted
    -- ^ /Restricted/ characters should be treated with caution when considering
    -- possible use in identifiers, and should be disallowed unless there is
    -- good reason to allow them in the environment in question.
    | Allowed
    -- ^ /Allowed/ characters are not typically used as is by implementations.
    -- Instead, they are applied as filters to the set of supported characters.
    deriving (Eq, Ord, Bounded, Enum, Show)

-- [TODO] @since
-- | Returns the 'IdentifierStatus' corresponding to a character.
--
{-# INLINE identifierStatus #-}
identifierStatus :: Char -> IdentifierStatus
identifierStatus c = if S.isAllowedIdentifier c
    then Allowed
    else Restricted

-- [TODO] @since
-- | Returns 'True' if the given character is 'Allowed' in an identifier.
--
{-# INLINE isAllowedIdentifier #-}
isAllowedIdentifier :: Char -> Bool
isAllowedIdentifier = S.isAllowedIdentifier

-- [TODO] @since
-- | Get the 'IdentifierStatus' of an 'T.IdentifierType'.
{-# INLINE identifierTypeStatus #-}
identifierTypeStatus :: T.IdentifierType -> IdentifierStatus
identifierTypeStatus = \case
    T.Inclusion   -> Allowed
    T.Recommended -> Allowed
    _             -> Restricted

-- [TODO] @since
-- | Returns the 'IdentifierType's corresponding to a character.
{-# INLINE identifierTypes #-}
identifierTypes :: Char -> NonEmpty T.IdentifierType
identifierTypes = T.decodeIdentifierTypes . T.identifierTypes

-- [TODO] @since
-- | Returns the /prototype/ of a character if it is /unintentionally/
-- confusable, else 'Nothing'.
{-# INLINE prototypeM #-}
prototypeM :: Char -> Maybe String
prototypeM = fmap decode . C.prototypeM
    where
    decode = unsafePerformIO . Foreign.peekCString Encoding.utf8

-- [TODO] @since
-- | Returns the /prototype/ of a character.
--
-- Note: returns the character itself if it is not /unintentionally/ confusable.
{-# INLINE prototype #-}
prototype :: Char -> String
prototype c = fromMaybe [c] (prototypeM c)

-- [TODO] @since
-- | Returns the list of /intentional/ confusables of a character, if any.
{-# INLINE intentionalConfusables #-}
intentionalConfusables :: Char -> String
intentionalConfusables = maybe mempty decode . IC.intentionalConfusables
    where
    decode = unsafePerformIO . Foreign.peekCString Encoding.utf8

-- [TODO] @since
-- | Returns 'True' if the character is /intentionally/ confusable.
{-# INLINE isIntentionallyConfusable #-}
isIntentionallyConfusable :: Char -> Bool
isIntentionallyConfusable = isJust . IC.intentionalConfusables
