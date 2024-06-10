{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
--
module UCD2Haskell.Common
    ( -- * Fold
      Fold(..)
    , distribute
    , filterFold
    , rmapFold
    , runFold

    -- * Formatting
    , showB
    , showPaddedHex
    , showPaddedHexB
    , showPaddedHeX
    , showPaddedHeXB
    , showHexCodepoint
    , showHexCodepointB
    , showHexCodepointBS

    -- * Hangul
    , jamoLCount
    , jamoVCount
    , jamoTCount
    , hangulFirst
    , hangulLast
    , isHangul
    , isHangulRange
    , filterNonHangul

      -- * Miscellaneous
    , Version(..)
    , allRange
    , mkHaskellConstructor
    ) where

import Data.Foldable (Foldable(..))
import qualified Data.Version as V
import Numeric (showHex)
import Data.Char (toUpper, ord, isAlphaNum)
import qualified Data.ByteString.Builder as BB
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as B8
import qualified Text.ParserCombinators.ReadP as P
import WithCli (Argument(..), HasArguments (..), atomicArgumentsParser)

--------------------------------------------------------------------------------
-- Fold that mimimc Streamly’s one
--------------------------------------------------------------------------------

data Fold a b = forall s. Fold
  { _step :: s -> a -> s
  , _initial :: s
  , _final :: s -> b }

data Pair a b = Pair !a !b

teeWith :: (a -> b -> c) -> Fold x a -> Fold x b -> Fold x c
teeWith f (Fold stepL initialL finalL) (Fold stepR initialR finalR) =
  Fold step initial final
  where
    step (Pair sL sR) x = Pair (stepL sL x) (stepR sR x)
    initial = Pair initialL initialR
    final (Pair sL sR) = f (finalL sL) (finalR sR)

distribute :: [Fold a b] -> Fold a [b]
distribute = foldr (teeWith (:)) (Fold const () (const []))

{-# INLINE filterFold #-}
filterFold :: (a -> Bool) -> Fold a b -> Fold a b
filterFold p (Fold step initial done) = Fold step' initial done
  where
    step' s a = if p a then step s a else s

{-# INLINE rmapFold #-}
rmapFold :: (b -> c) -> Fold a b -> Fold a c
rmapFold f (Fold step initial final) = Fold step initial (f . final)

runFold :: Fold a b -> [a] -> b
runFold (Fold step initial final) = final . foldl' step initial

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

-- | /Warning:/ the use of 'BB.string7' make it unsafe if applied to non-ASCII.
showB :: (Show a) => a -> BB.Builder
showB = BB.string7 . show

showPaddedHex :: Int -> String
showPaddedHex cp =
    let hex = showHex cp mempty
        padding = 4 - length hex
    in replicate padding '0' <> hex

showPaddedHexB :: Int -> BB.Builder
showPaddedHexB = BB.string7 . showPaddedHex

showPaddedHeX :: Int -> String
showPaddedHeX = fmap toUpper . showPaddedHex

showPaddedHeXB :: Int -> BB.Builder
showPaddedHeXB = BB.string7 . showPaddedHeX

showHexCodepoint :: Char -> String
showHexCodepoint = showPaddedHeX . ord

showHexCodepointB :: Char -> BB.Builder
showHexCodepointB = BB.string7 . showHexCodepoint

showHexCodepointBS :: Char -> BS.ShortByteString
showHexCodepointBS = BS.toShort . B8.pack . showPaddedHeX . ord

--------------------------------------------------------------------------------
-- Hangul
--------------------------------------------------------------------------------

-- This bit of code is duplicated but this duplication allows us to reduce 2
-- dependencies on the executable.

jamoLCount :: Int
jamoLCount = 19

jamoVCount :: Int
jamoVCount = 21

jamoTCount :: Int
jamoTCount = 28

hangulFirst :: Int
hangulFirst = 0xac00

hangulLast :: Int
hangulLast = hangulFirst + jamoLCount * jamoVCount * jamoTCount - 1

isHangul :: Char -> Bool
isHangul c = n >= hangulFirst && n <= hangulLast
    where n = ord c

isHangulRange :: U.CodePointRange -> Bool
isHangulRange = \case
    U.SingleChar c -> isHangul c
    U.CharRange start end ->
        ord start >= hangulFirst && ord end <= hangulLast

filterNonHangul :: Fold UD.Entry a -> Fold UD.Entry a
filterNonHangul = filterFold (not . isHangulRange . UD.range)

--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

allRange :: (Char -> Bool) -> U.CodePointRange -> Bool
allRange predicate = \case
    U.SingleChar c -> predicate c
    U.CharRange start end -> all predicate [start..end]

-- -- Make a valid Haskell constructor (in CamelCase) from an identifier.
-- mkHaskellConstructor :: String -> String
-- mkHaskellConstructor = reverse . fst . foldl' convert (mempty, True)
--     where

--     convert (acc, newWord) = \case
--         -- Skip the following and start a new word
--         ' ' -> (acc, True)
--         '-' -> (acc, True)
--         '_' -> (acc, True)
--         -- Letter or number
--         c   -> if isAscii c && isAlphaNum c
--             then ( if newWord then toUpper c : acc else c : acc
--                  , False)
--             else error ("Unsupported character: " <> show c)

-- Make a valid Haskell constructor (in CamelCase) from an identifier.
mkHaskellConstructor :: BS.ShortByteString -> BB.Builder
mkHaskellConstructor = fst . BS.foldl' convert (mempty, True)
    where

    convert (acc, newWord) = \case
        -- Skip the following and start a new word
        0x20 -> (acc, True) -- Space
        0x2d -> (acc, True) -- Hyphen
        0x5f -> (acc, True) -- Underscore
        -- Letter or number
        c   -> if isAlphaNum (word82Char c)
            then ( acc <> BB.word8 if newWord then toUpper' c else c
                 , False )
            else error ("Unsupported character: " <> show (word82Char c))
    word82Char = toEnum . fromIntegral
    char2Word8 = fromIntegral . fromEnum
    toUpper' = char2Word8 . toUpper . word82Char

-- | Used to parse Unicode version as CLI argument
newtype Version = Version { unVersion :: V.Version }
    deriving newtype Show

instance Argument Version where
    argumentType _ = "STRING"
    parseArgument raw = case P.readP_to_S (V.parseVersion <* P.eof) raw of
        [(v, "")] -> Just (Version v)
        _ -> Nothing

instance HasArguments Version where
    argumentsParser = atomicArgumentsParser
