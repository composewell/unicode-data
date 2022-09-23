{-# LANGUAGE LambdaCase, CPP #-}

module Text.Case
    ( toUpperStream
    , toLowerStream
    , toCaseFoldStream
#if MIN_VERSION_text(2,0,0)
    , toUpperText
    , toLowerText
    , toCaseFoldText
#endif
    ) where

import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as TF
import qualified Data.Text.Internal.Fusion.Size as TF
import qualified Unicode.Char.Case as C

#if MIN_VERSION_text(2,0,0)

import           Control.Monad.ST (ST, runST)
import           Data.Word (Word8)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as T
import           Data.Text.Internal.Encoding.Utf8 (utf8LengthByLeader, chr2, chr3, chr4)
import           Data.Text.Internal.Unsafe.Char (unsafeWrite)

#endif

-- Text case operations

data CC s a = CC !s !(C.Step a Char)

-- [NOTE] Adapted from Data.Text.Internal.Fusion.Common
streamUnfold :: C.Unfold Char Char -> TF.Stream Char -> TF.Stream Char
streamUnfold (C.Unfold step inject) = \case
    TF.Stream next0 s0 len ->
        TF.Stream next (CC s0 C.Stop) (len `TF.unionSize` (3*len))
        where
            next (CC s C.Stop) = case next0 s of
                TF.Done       -> TF.Done
                TF.Skip s'    -> TF.Skip (CC s' C.Stop)
                TF.Yield c s' -> case inject c of
                    C.Yield c' st -> TF.Yield c' (CC s' (step st))
                    C.Last  c'    -> TF.Yield c' (CC s' C.Stop)
                    -- impossible: there is always at least one Char
                    C.Stop        -> TF.Skip (CC s' C.Stop)
            next (CC s (C.Yield c st)) = TF.Yield c (CC s (step st))
            next (CC s (C.Last  c   )) = TF.Yield c (CC s C.Stop)

caseConvertStream :: C.Unfold Char Char -> T.Text -> T.Text
caseConvertStream u t = TF.unstream (streamUnfold u (TF.stream t))

{-# INLINE toUpperStream #-}
toUpperStream :: T.Text -> T.Text
toUpperStream = caseConvertStream C.upperCaseMapping

{-# INLINE toLowerStream #-}
toLowerStream :: T.Text -> T.Text
toLowerStream = caseConvertStream C.lowerCaseMapping

{-# INLINE toCaseFoldStream #-}
toCaseFoldStream :: T.Text -> T.Text
toCaseFoldStream = caseConvertStream C.caseFoldMapping

#if MIN_VERSION_text(2,0,0)

-- [NOTE] Adapted from Data.Text
caseConvertText :: (Word8 -> Word8) -> C.Unfold Char Char -> T.Text -> T.Text
caseConvertText ascii (C.Unfold (step :: u -> C.Step u Char) inject) (T.Text src o l) = runST $ do
  -- Case conversion a single code point may produce up to 3 code-points,
  -- each up to 4 bytes, so 12 in total.
  dst <- A.new (l + 12)
  outer dst l o 0
  where
    outer :: forall s. A.MArray s -> Int -> Int -> Int -> ST s T.Text
    outer !dst !dstLen = inner
      where
        inner !srcOff !dstOff
          | srcOff >= o + l = do
            A.shrinkM dst dstOff
            arr <- A.unsafeFreeze dst
            return (T.Text arr 0 dstOff)
          | dstOff + 12 > dstLen = do
            -- Ensure to extend the buffer by at least 12 bytes.
            let !dstLen' = dstLen + max 12 (l + o - srcOff)
            dst' <- A.resizeM dst dstLen'
            outer dst' dstLen' srcOff dstOff
          -- If a character is to remain unchanged, no need to decode Char back into UTF8,
          -- just copy bytes from input.
          | otherwise = do
            let m0 = A.unsafeIndex src srcOff
                m1 = A.unsafeIndex src (srcOff + 1)
                m2 = A.unsafeIndex src (srcOff + 2)
                m3 = A.unsafeIndex src (srcOff + 3)
                !d = utf8LengthByLeader m0
            case d of
              1 -> do
                A.unsafeWrite dst dstOff (ascii m0)
                inner (srcOff + 1) (dstOff + 1)
              2 -> do
                let !c = chr2 m0 m1
                dstOff' <- case inject c of
                  -- Unchanged
                  C.Last _ -> do
                    A.unsafeWrite dst dstOff m0
                    A.unsafeWrite dst (dstOff + 1) m1
                    pure $ dstOff + 2
                  st -> writeMapping st dstOff
                inner (srcOff + 2) dstOff'
              3 -> do
                let !c = chr3 m0 m1 m2
                dstOff' <- case inject c of
                  -- Unchanged
                  C.Last _ -> do
                    A.unsafeWrite dst dstOff m0
                    A.unsafeWrite dst (dstOff + 1) m1
                    A.unsafeWrite dst (dstOff + 2) m2
                    pure $ dstOff + 3
                  st -> writeMapping st dstOff
                inner (srcOff + 3) dstOff'
              _ -> do
                let !c = chr4 m0 m1 m2 m3
                dstOff' <- case inject c of
                  -- Unchanged
                  C.Last _ -> do
                    A.unsafeWrite dst dstOff m0
                    A.unsafeWrite dst (dstOff + 1) m1
                    A.unsafeWrite dst (dstOff + 2) m2
                    A.unsafeWrite dst (dstOff + 3) m3
                    pure $ dstOff + 4
                  st -> writeMapping st dstOff
                inner (srcOff + 4) dstOff'

        writeMapping :: C.Step u Char -> Int -> ST s Int
        writeMapping = \case
            C.Stop        -> pure
            C.Yield ch st -> \dstOff -> do
                d <- unsafeWrite dst dstOff ch
                writeMapping (step st) (dstOff + d)
            C.Last  ch    -> \dstOff -> do
                d <- unsafeWrite dst dstOff ch
                pure (dstOff + d)
{-# INLINE caseConvertText #-}

{-# INLINE toUpperText #-}
toUpperText :: T.Text -> T.Text
toUpperText = caseConvertText
    (\w -> if w - 97 <= 25 then w - 32 else w)
    C.upperCaseMapping

{-# INLINE toLowerText #-}
toLowerText :: T.Text -> T.Text
toLowerText = caseConvertText
    (\w -> if w - 65 <= 25 then w + 32 else w)
    C.lowerCaseMapping

{-# INLINE toCaseFoldText #-}
toCaseFoldText :: T.Text -> T.Text
toCaseFoldText = caseConvertText
    (\w -> if w - 65 <= 25 then w + 32 else w)
    C.caseFoldMapping

#endif
