-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.Properties
    ( propList
    , derivedCoreProperties)
    where

import Control.Exception (assert)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
import Data.Char (chr, ord)
import qualified Data.IntSet as IntSet
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Multiple as Props

import UCD2Haskell.Common (Fold (..))
import UCD2Haskell.Generator (
    FileRecipe (..),
    apacheLicense,
    genBitmapShamochu,
    unlinesBB,
 )

propList :: Set.Set BS.ShortByteString -> FileRecipe Props.Entry
propList props = ModuleRecipe
    "Unicode.Internal.Char.PropList"
    (`genCorePropertiesModule` (`elem` props))

derivedCoreProperties :: Set.Set BS.ShortByteString -> FileRecipe Props.Entry
derivedCoreProperties props = ModuleRecipe
    "Unicode.Internal.Char.DerivedCoreProperties"
    (`genCorePropertiesModule` (`elem` props))

data Acc = Acc
    { properties :: ![BS.ShortByteString]
    , values :: !(Map.Map BS.ShortByteString IntSet.IntSet) }

genCorePropertiesModule :: BB.Builder -> (BS.ShortByteString -> Bool) -> Fold Props.Entry BB.Builder
genCorePropertiesModule moduleName isProp = Fold step initial done
    where

    prop2FuncName = ("is" <>) . BB.shortByteString
    prop2FuncNameStr = ("is" <>) . fmap (chr . fromIntegral) . BS.unpack

    initial = Acc mempty mempty

    step acc@Acc{..} (Props.Entry range property pValues)
        | not (isProp property) = acc -- Skip property
        | otherwise = assert (isNothing pValues) case range of
            U.SingleChar c -> Acc
                { properties = if property `elem` properties
                    then properties
                    else property : properties
                , values = addChar property (ord c) values }
            U.CharRange{..} -> Acc
                { properties = if property `elem` properties
                    then properties
                    else property : properties
                , values = addChars
                    property
                    (IntSet.fromDistinctAscList [ord start..ord end])
                    values }

    addChar property c = flip Map.alter property \case
        Nothing -> Just (IntSet.singleton c)
        Just cs -> Just (IntSet.insert c cs)

    addChars property xs = flip Map.alter property \case
        Nothing -> Just xs
        Just ys -> Just (xs <> ys)

    done Acc{..} = unlinesBB (header properties <> genBitmaps values properties)

    genBitmaps values = foldr addBitMap mempty
        where
        addBitMap property =
            (:)
            (genBitmapShamochu
                (prop2FuncNameStr property)
                (5 NE.:| [6, 7])
                -- [2,3,4,5,6]
                []
                (IntSet.toAscList (values Map.! property)))

    header exports =
        [ apacheLicense 2020 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , "{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}"
        , "{-# HLINT ignore \"Use camelCase\" #-}"
        , ""
        , "module " <> moduleName
        , "    ( "
            <> mconcat (L.intersperse "\n    , " (map prop2FuncName exports))
        , "    ) where"
        , ""
        , "import Data.Bits (Bits(..))"
        , "import Data.Char (ord)"
        , "import Data.Int (Int8)"
        , "import Data.Word (Word8, Word16)"
        , "import GHC.Exts (Ptr(..))"
        , "import Unicode.Internal.Bits (lookupBit, lookupWord16AsInt, lookupWord8AsInt)"
        , ""
        ]
