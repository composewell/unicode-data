{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}

-- |
-- Module      : Unicode.Internal.Unfold
-- Copyright   : (c) 2022 Composewell Technologies and Contributors
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- @since 0.3.1
--

module Unicode.Internal.Unfold
    ( Unfold(..)
    , Step(..)
    , toList
    ) where

-- | An @Unfold a b@ is a generator of a stream of values of type @b@ from a
-- seed of type @a@.
--
-- @since 0.3.1
#if MIN_VERSION_base(4,12,0)
data Unfold a b = forall s. Unfold
    (s -> Step s b)
    -- ^ /Step/ function: compute the next step from the current one.
    (a -> Step s b)
    -- ^ /Inject/ function: initialize the state with a seed value.
#else
data Unfold a b =
    -- | @Unfold step inject@
    forall s. Unfold (s -> Step s b) (a -> Step s b)
#endif

-- | A stream is a succession of 'Step's.
--
-- @since 0.3.1
data Step s a
    = Yield !a !s
    -- ^ Produces a single value and the next state of the stream.
    | Stop
    -- ^ Indicates there are no more values in the stream.

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap f (Yield x s) = Yield (f x) s
    fmap _ Stop        = Stop

-- | Convert an 'Unfold a a' to a list [a], if the resulting list is empty the
-- seed is used as a default output.
--
{-# INLINE toList #-}
toList :: Unfold a a -> a -> [a]
toList (Unfold step inject) input =
    case inject input of
        Stop -> [input]
        Yield b s -> b : go (step s)
    where
    go = \case
        Yield b s -> let !s' = step s in b : go s'
        Stop      -> []
