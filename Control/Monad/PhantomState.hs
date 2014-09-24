
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
--   any value. Therefore, it automatically discards
--   the result of any computation. Only changes in
--   the state and effects will remain. The primitive
--   operations in this monad are:
--
-- * 'useState': Performs an effect. State is unchanged.
-- * 'changeState': Changes state. No effect is performed.
--
--   These two functions complement each other. If you want
--   to perform an effect /and/ change the state, build your
--   action using the 'PhantomStateT' constructor.
--
--   Use 'runPhantomStateT' (or 'runPhantomState') to get
--   the result of a phantom state computation.
--
--   As stated before, 'PhantomStateT' is a monad transformer.
--   Therefore, it is an instance of the 'MonadTrans' class.
--   In this case, 'lift' @m@ performs the same action as @m@,
--   but returning no value. It captures the effects in @m@
--   and nothing else.
--
newtype PhantomStateT s m a = PhantomStateT (s -> m s)

-- | Type synonym of 'PhantomStateT' where the underlying 'Monad' is the 'Identity' monad.
type PhantomState s = PhantomStateT s Identity

-- | Perform an applicative action using the current state, leaving
--   the state unchanged. The result will be discarded, so only the
--   effect will remain.
useState :: Applicative m => (s -> m a) -> PhantomStateT s m ()
{-# INLINE useState #-}
useState f = PhantomStateT $ \x -> f x *> pure x

-- | Modify the state using a pure function. No effect will be produced,
--   only the state will be modified.
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
