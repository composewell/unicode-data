{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}

-- [TODO] @since
-- |
-- Module      : Unicode.Internal.Unfold
-- Copyright   : (c) 2022 Composewell Technologies and Contributors
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Unicode.Internal.Unfold
    ( Unfold(..)
    , Step(..)
    , toList
    ) where

-- [TODO] @since
-- | An @Unfold a b@ is a generator of a stream of values of type @b@ from a
-- seed of type @a@.
data Unfold a b =
    -- | @Unfold step inject@
    forall s. Unfold (s -> Step s b) (a -> s)

-- [TODO] @since
-- | A stream is a succession of 'Step's.
data Step s a
    = Yield !a !s
    -- ^ Produces a single value and the next state of the stream.
    | Stop
    -- ^ Indicates there are no more values in the stream.

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap f (Yield x s) = Yield (f x) s
    fmap _ Stop        = Stop

-- [TODO] @since
-- | Convert an 'Unfold' to a list.
{-# INLINE toList #-}
toList :: Unfold a b -> a -> [b]
toList (Unfold step inject) a = go (inject a)
    where
    go s = case step s of
        Yield b s' -> b : go s'
        Stop       -> []