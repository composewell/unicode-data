-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements.  See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership.  The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- "License"); you may not use this file except in compliance
-- with the License.  You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied.  See the License for the
-- specific language governing permissions and limitations
-- under the License.

-- |
-- Module      : Data.Unicode.Properties.Decompose
--
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Data.Unicode.Properties.Decompose
    ( decomposeChar
    , decomposeCharHangul
    , DecomposeMode(..)
    , isHangul
    , jamoLFirst
    , isDecomposable
    )
where

import qualified Data.Unicode.Properties.Decomposable    as D
import qualified Data.Unicode.Properties.DecomposableK   as K
import           Data.Unicode.Properties.DecomposeHangul ( decomposeCharHangul
                                                         , jamoLFirst
                                                         , isHangul)
import qualified Data.Unicode.Properties.Decompositions  as D
import qualified Data.Unicode.Properties.DecompositionsK as K

data DecomposeMode = DecomposeNFD | DecomposeNFKD

{-# INLINE decomposeChar #-}
decomposeChar :: DecomposeMode -> Char -> [Char]
decomposeChar DecomposeNFD  = D.decomposeChar
decomposeChar DecomposeNFKD = K.decomposeChar

{-# INLINE isDecomposable #-}
isDecomposable :: DecomposeMode -> Char -> Bool
isDecomposable DecomposeNFD  = D.isDecomposable
isDecomposable DecomposeNFKD = K.isDecomposable
