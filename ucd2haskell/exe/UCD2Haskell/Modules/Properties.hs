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
import Data.Char (ord)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Multiple as Props

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense, genBitmap, unwordsBB)
import UCD2Haskell.Common (Fold (..))

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
            addBitMap property = (:)
                (genBitmap (prop2FuncName property)
                           (IntSet.toAscList (values Map.! property)))

    header exports =
        [ apacheLicense 2020 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(" <> unwordsBB (L.intersperse "," (map prop2FuncName exports)) <> ")"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Data.Word (Word8)"
        , "import GHC.Exts (Ptr(..))"
        , "import Unicode.Internal.Bits (lookupBit64)"
        , ""
        ]
