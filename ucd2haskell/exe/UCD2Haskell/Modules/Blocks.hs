-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Modules.Blocks (recipe) where

import qualified Data.ByteString.Builder as BB
import Data.Char (ord)
import Data.Bits (Bits(..))
import qualified Data.List as L
import Data.Word (Word8, Word32)
import qualified Unicode.CharacterDatabase.Parser.Common as U
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop

import UCD2Haskell.Generator (FileRecipe (..), unlinesBB, apacheLicense, word32ToWord8s, enumMapToAddrLiteral)
import UCD2Haskell.Common (Fold (..), showPaddedHexB, showPaddedHeXB, mkHaskellConstructor)

recipe :: FileRecipe Prop.Entry
recipe = ModuleRecipe
    "Unicode.Internal.Char.Blocks"
    genBlocksModule

data Acc = Acc
    { blocks :: ![BB.Builder]
    , defs :: ![BB.Builder]
    , ranges :: ![(Int, Int)] }

genBlocksModule :: BB.Builder -> Fold Prop.Entry BB.Builder
genBlocksModule moduleName = Fold step initial done
    where

    done Acc{..} = let ranges' = reverse ranges in unlinesBB
        [ apacheLicense 2022 moduleName
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        , "(Block(..), BlockDefinition(..), block, blockDefinition)"
        , "where"
        , ""
        , "import Data.Ix (Ix)"
        , "import Data.Word (Word32)"
        , "import GHC.Exts"
        , "import Unicode.Internal.Bits (lookupWord32#)"
        , ""
        , "-- | Unicode [block](https://www.unicode.org/glossary/#block)."
        , "--"
        , "-- There is a total of " <> BB.intDec (length blocks) <> " blocks."
        , "--"
        , "-- @since 0.3.1"
        , "data Block"
        , "    = " <> mconcat (L.intersperse "\n    | " (reverse blocks))
        , "    deriving (Enum, Bounded, Eq, Ord, Ix, Show)"
        , ""
        , "-- | Block definition: range and name."
        , "--"
        , "-- @since 0.3.1"
        , "data BlockDefinition = BlockDefinition"
        , "    { blockRange :: !(Int, Int) -- ^ Range"
        , "    , blockName :: !String -- ^ Name"
        , "    } deriving (Eq, Ord, Show)"
        , ""
        , "-- | Block definition"
        , "--"
        , "-- @since 0.3.1"
        , "blockDefinition :: Block -> BlockDefinition"
        , "blockDefinition b = case b of"
        , mconcat (reverse defs)
        , "-- | Character block, if defined."
        , "--"
        , "-- @since 0.3.1"
        , "block :: Char -> Maybe Int"
        , "block (C# c#) = getBlock 0# " <> BB.intDec (length ranges - 1) <> BB.char7 '#'
        , "    where"
        , "    -- [NOTE] Encoding"
        , "    -- A range is encoded as two LE Word32:"
        , "    -- • First one is the lower bound, where the higher 11 bits are the block"
        , "    --   index and the lower 21 bits are the codepoint."
        , "    -- • Second one is the upper bound, which correspond to the codepoint."
        , ""
        , "    cp# = int2Word# (ord# c#)"
        , ""
        , "    -- Binary search"
        , "    getBlock l# u# = if isTrue# (l# ># u#)"
        , "        then Nothing"
        , "        else"
        , "            let k# = l# +# uncheckedIShiftRL# (u# -# l#) 1#"
        , "                j# = k# `uncheckedIShiftL#` 1#"
        , "                cpL0# = getRawCodePoint# j#"
        , "                cpL# = cpL0# `and#` 0x1fffff## -- Mask for codepoint: [0..0x10fff]"
        , "                cpU# = getRawCodePoint# (j# +# 1#)"
        , "            in if isTrue# (cpU# `ltWord#` cp#)"
        , "                -- cp > upper bound"
        , "                then getBlock (k# +# 1#) u#"
        , "                -- check lower bound"
        , "                else if isTrue# (cp# `ltWord#` cpL#)"
        , "                    -- cp < lower bound"
        , "                    then getBlock l# (k# -# 1#)"
        , "                    -- cp in block: get block index"
        , "                    else let block# = cpL0# `uncheckedShiftRL#` 21#"
        , "                         in Just (I# (word2Int# block#))"
        , ""
        , "    getRawCodePoint# = lookupWord32# ranges#"
        , ""
        , "    -- Encoded ranges"
        , "    !(Ptr ranges#) = rangesBitmap"
        , ""
        , "rangesBitmap :: Ptr Word32"
        , "rangesBitmap = Ptr"
        , "    \"" <> enumMapToAddrLiteral 4 0xff (mkRanges ranges') "\"#"
        ]

    initial = Acc mempty mempty mempty

    step Acc{..} (Prop.Entry range blockName) = case range of
        U.SingleChar c -> error ("genBlocksModule: expected range, got: " <> show c)
        U.CharRange start end ->
            let blockID = mkHaskellConstructor blockName
                blockRange = (ord start, ord end)
                blockName' = BB.shortByteString blockName
             in Acc
                { blocks = mkBlockConstructor blockID blockName' blockRange : blocks
                , defs = mkBlockDef   blockID blockName' blockRange : defs
                , ranges = blockRange : ranges }

    mkBlockConstructor blockID blockName (l, u) = mconcat
        [ blockID
        , " -- ^ @U+"
        , showPaddedHeXB l
        , "..U+"
        , showPaddedHeXB u
        , "@: "
        , blockName
        , "."
        ]

    mkBlockDef blockID blockName (l, u) = mconcat
        [ "    "
        , blockID
        , " -> BlockDefinition (0x"
        , showPaddedHexB l
        , ", 0x"
        , showPaddedHexB u
        , ") \""
        , blockName
        , "\"\n"
        ]

    -- [NOTE] Encoding: a range is encoded as two LE Word32:
    -- • First one is the lower bound, where the higher 11 bits are the block
    --   index and the lower 21 bits are the codepoint.
    -- • Second one is upper bound, which correspond to the codepoint.
    mkRanges :: [(Int, Int)] -> [Word8]
    mkRanges = foldMap (uncurry mkBlockRange) . zip [0..]

    mkBlockRange :: Word32 -> (Int, Int) -> [Word8]
    mkBlockRange idx (l, u) = encodeBound idx l <> encodeBound 0 u

    encodeBound :: Word32 -> Int -> [Word8]
    encodeBound idx n = word32ToWord8s ((idx `shiftL` 21) .|. fromIntegral n)
