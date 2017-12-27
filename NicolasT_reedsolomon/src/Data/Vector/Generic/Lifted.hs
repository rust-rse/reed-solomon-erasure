module Data.Vector.Generic.Lifted (
      unsafeFreeze
    , read
    , write
    , new
    , freeze
    , replicate
    , mapM
    , thaw
    , forM
    ) where

import Prelude hiding (mapM, read, replicate)

import Control.Monad.Trans (MonadTrans, lift)

import Data.Vector.Generic (Mutable, Vector)
import qualified Data.Vector.Generic as V
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV
import Control.Monad.Primitive (PrimMonad, PrimState)

unsafeFreeze :: (Vector v a, PrimMonad m, MonadTrans t) => Mutable v (PrimState m) a -> t m (v a)
unsafeFreeze = lift . V.unsafeFreeze
{-# INLINE unsafeFreeze #-}

read :: (MVector v a, PrimMonad m, MonadTrans t) => v (PrimState m) a -> Int -> t m a
read v i = lift (MV.read v i)
{-# INLINE read #-}

write :: (MVector v a, PrimMonad m, MonadTrans t) => v (PrimState m) a -> Int -> a -> t m ()
write v i a = lift (MV.write v i a)
{-# INLINE write #-}

new :: (MVector v a, PrimMonad m, MonadTrans t) => Int -> t m (v (PrimState m) a)
new = lift . MV.new
{-# INLINE new #-}

freeze :: (Vector v a, PrimMonad m, MonadTrans t) => Mutable v (PrimState m) a -> t m (v a)
freeze = lift . V.freeze
{-# INLINE freeze #-}

replicate :: (MVector v a, PrimMonad m, MonadTrans t) => Int -> a -> t m (v (PrimState m) a)
replicate i a = lift (MV.replicate i a)

mapM :: (Vector v a, Vector v b, Monad m, MonadTrans t) => (a -> m b) -> v a -> t m (v b)
mapM f v = lift $ V.mapM f v
{-# INLINE mapM #-}

thaw :: (Vector v a, PrimMonad m, MonadTrans t) => v a -> t m (Mutable v (PrimState m) a)
thaw = lift . V.thaw
{-# INLINE thaw #-}

forM :: (Vector v a, Vector v b, Monad m, MonadTrans t) => v a -> (a -> m b) -> t m (v b)
forM = flip mapM
{-# INLINE forM #-}
