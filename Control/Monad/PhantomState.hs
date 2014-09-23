
-- | Phantom Monad State Transformer constructor and functions.
module Control.Monad.PhantomState (
    PhantomStateT (..)
  , PhantomState
  , useState
  , changeState
  , runPhantomStateT
  , runPhantomState
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Data.Functor.Identity

-- | The Phantom State Monad Transformer is like the
--   State Monad Transformer, but it does not hold
--   any value.
newtype PhantomStateT s m a = PhantomStateT (s -> m s)

-- | Type synonym of 'PhantomStateT' where the underlying 'Monad' is the 'Identity' monad.
type PhantomState s a = PhantomStateT s Identity a

-- | Perform an applicative action using the current state, leaving
--   the state unchanged.
useState :: Applicative m => (s -> m a) -> PhantomStateT s m ()
{-# INLINE useState #-}
useState f = PhantomStateT $ \x -> f x *> pure x

-- | Modify the state using a pure function.
changeState :: Applicative m => (s -> s) -> PhantomStateT s m ()
{-# INLINE changeState #-}
changeState f = PhantomStateT $ pure . f

-- | Perform a phantom state computation by setting an initial state
--   and running all the actions from there.
runPhantomStateT :: PhantomStateT s m a -- ^ Phantom state computation
                 -> s -- ^ Initial state
                 -> m s -- ^ Final result
{-# INLINE runPhantomStateT #-}
runPhantomStateT (PhantomStateT f) x = f x

-- | Specialized version of 'runPhantomStateT' where the underlying
--   'Monad' is the 'Identity' monad.
runPhantomState :: PhantomState s a -- ^ Phantom state computation
                -> s -- ^ Initial state
                -> s -- ^ Final result
{-# INLINE runPhantomState #-}
runPhantomState f = runIdentity . runPhantomStateT f

-- Instances

instance Functor (PhantomStateT s m) where
  {-# INLINE fmap #-}
  fmap _ (PhantomStateT f) = PhantomStateT f

instance Monad m => Applicative (PhantomStateT s m) where
  {-# INLINE pure #-}
  pure _ = PhantomStateT return
  {-# INLINE (<*>) #-}
  PhantomStateT f <*> PhantomStateT g = PhantomStateT (\x -> f x >>= g)
  {-# INLINE  (*>) #-}
  PhantomStateT f  *> PhantomStateT g = PhantomStateT (\x -> f x >>= g)
  {-# INLINE (<*) #-}
  PhantomStateT f <*  PhantomStateT g = PhantomStateT (\x -> f x >>= g)

instance Monad m => Monad (PhantomStateT s m) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  x >>= f = x *> f undefined
  {-# INLINE (>>) #-}
  (>>) = (*>)

instance MonadTrans (PhantomStateT s) where
  {-# INLINE lift #-}
  lift m = PhantomStateT (\x -> m >> return x)
