{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Generic.Exceptions (
      CatchST
    , runCatchST
    , create
    , modify
    ) where

import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)

import Control.Monad.Catch.Pure (CatchT(runCatchT), MonadThrow, SomeException(..), throwM)

import Data.Vector.Generic (Vector, Mutable, clone)
import Data.Vector.Generic.Lifted (unsafeFreeze)
import qualified Data.Vector.Generic.New as N

throwEither :: (MonadThrow m)
            => Either SomeException v
            -> m v
throwEither = either throwM return
{-# INLINE throwEither #-}

type CatchST s a = CatchT (ST s) a

runCatchST :: MonadThrow m => (forall s. CatchST s a) -> m a
runCatchST a = throwEither $ runST $ runCatchT a
{-# INLINE runCatchST #-}

new :: Vector v a
    => (forall s. CatchST s (Mutable v s a))
    -> Either SomeException (v a)
new n = n `seq` runST $ runCatchT (n >>= unsafeFreeze)
{-# INLINE new #-}

create :: (Vector v a, MonadThrow m)
       => (forall s. CatchST s (Mutable v s a))
       -> m (v a)
create p = throwEither $ new p
{-# INLINE create #-}

modify :: (Vector v a, MonadThrow m)
       => (forall s. Mutable v s a -> CatchST s ())
       -> v a
       -> m (v a)
modify f v = create $ modify' $ clone v
  where
    modify' n = do
        n' <- lift $ N.run n
        f n'
        return n'
    {-# INLINE modify' #-}
{-# INLINE modify #-}
