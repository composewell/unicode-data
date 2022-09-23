{-# LANGUAGE LambdaCase #-}

module Text.Case
    ( toUpperStream
    , toLowerStream
    , toCaseFoldStream
    ) where

import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as TF
import qualified Data.Text.Internal.Fusion.Size as TF
import qualified Unicode.Char.Case as C

-- Text case operations

data CC s a = CC !s !(C.Step a Char)

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
                    -- impossible: there is always at least one Char
                    C.Stop        -> TF.Skip (CC s' C.Stop)
            next (CC s (C.Yield c st)) = TF.Yield c (CC s (step st))

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
