{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module : Data.Vector.Generic.Sized
-- Description : Type-indexed vectors
-- Copyright : (C) 2015 Nicolas Trangez
-- License : MIT (see the file LICENSE)
-- Maintainer : Nicolas Trangez <ikke@nicolast.be>
-- Stability : experimental
--
-- This module implements type-indexed fixed-size generic vectors and related
-- utility functions.

module Data.Vector.Generic.Sized (
      Vector, SVector, UVector
    , fromVector
    , toVector
    , length
    , Bounded(..)
    , index
    , unsafeWith
    , generate
    , replicateM
    ) where

import Prelude hiding (Bounded, length)
import qualified Prelude

import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4, 8, 0)
import Data.Monoid (mconcat)
#endif
import Data.Proxy
import Data.Typeable
import Data.Word
import Foreign.Ptr (Ptr)
import GHC.Exts (IsList(..))
import GHC.TypeLits

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

-- | Length-indexed generic vectors.
newtype Vector v (n :: Nat) a = Vector (v a)
  deriving (Show, Eq, Ord, Typeable)

instance (V.Vector v a, KnownNat n) => IsList (Vector v n a) where
    type Item (Vector v n a) = a
    fromList l = fromMaybe exc $ fromVector $ V.fromList l
      where
        exc = error $ mconcat [ "fromList: Size mismatch, "
                              , show (Prelude.length l)
                              , " /= "
                              , show (natVal (Proxy :: Proxy n))
                              ]
    {-# INLINE fromList #-}
    fromListN n l =
        if n == fromInteger (natVal (Proxy :: Proxy n))
        then Vector (V.fromListN n l)
        else error $ mconcat [ "fromListN: Size mismatch, "
                             , show n
                             , " /= "
                             , show (natVal (Proxy :: Proxy n))
                             ]
    {-# INLINE fromListN #-}
    toList (Vector v) = V.toList v
    {-# INLINE toList #-}

-- | Type-indexed storable 'Data.Vector.Storable.Vector's.
type SVector n a = Vector SV.Vector n a
-- | Type-indexed unboxed 'Data.Vector.Unboxed.Vector's.
type UVector n a = Vector UV.Vector n a

-- | Turn a plain vector into a type-indexed 'Vector'.
--
-- If the length of the given vector doesn't match, 'Nothing' is returned.
--
-- O(1).
fromVector :: forall n v a. (KnownNat n, V.Vector v a)
           => v a -- ^ Plain vector
           -> Maybe (Vector v n a)
fromVector v =
    if V.length v == fromInteger (natVal (Proxy :: Proxy n))
    then Just (Vector v)
    else Nothing
{-# INLINE fromVector #-}

-- | Turn a type-indexed 'Vector' into a plain vector.
--
-- O(1).
toVector :: Vector v n a -> v a
toVector (Vector v) = v
{-# INLINE toVector #-}

-- | Retrieve the length of a type-indexed 'Vector'.
--
-- This is non-strict in the argument: the length calculation is
-- a compile-time operation based on the type of the 'Vector'.
--
-- O(0).
length :: forall n v a. KnownNat n => Vector v n a -> Word
length _ = fromInteger $ natVal (Proxy :: Proxy n)
{-# INLINE length #-}

-- | Type-level ('Nat') bounds of values of a type.
class (MinBound t <= MaxBound t) => Bounded t where
    type MinBound t :: Nat
    type MaxBound t :: Nat

instance Bounded Word8 where
    type MinBound Word8 = 0
    type MaxBound Word8 = 255

type a < b = CmpNat a b ~ 'LT
type a >= b = b <= a

-- | Safe indexing.
--
-- This function allows unchecked indexing into a 'Vector' whose length
-- must match at least the bounds of the given index type.
--
-- Assuming a correct 'Bounded' instance for 't', the lookup can never be
-- out-of-bounds, by construction.
--
-- O(1).
index :: (KnownNat n, MinBound t >= 0, MaxBound t < n, V.Vector v a, Integral t)
      => Vector v n a
      -> t -- ^ Index
      -> a
index (Vector v) !t = V.unsafeIndex v $ fromIntegral t
{-# INLINE index #-}

-- | Retrieve a pointer to a sized 'Data.Vector.Storable.Vector'.
--
-- See 'Data.Vector.Storable.unsafeWith'.
unsafeWith :: SV.Storable a => SVector n a -> (Ptr a -> IO b) -> IO b
unsafeWith (Vector v) = SV.unsafeWith v
{-# INLINE unsafeWith #-}

-- | Construct a sized 'Vector' by applying the function to each index.
--
-- O(n).
generate :: forall n v a. (V.Vector v a, KnownNat n) => (Int -> a) -> Vector v n a
generate = Vector . V.generate (fromInteger $ natVal (Proxy :: Proxy n))
{-# INLINE generate #-}

-- | Execute the monadic action to fill a new sized 'Vector'.
--
-- O(n).
replicateM :: forall m n v a. (Functor m, Monad m, V.Vector v a, KnownNat n) => m a -> m (Vector v n a)
replicateM = fmap Vector . V.replicateM (fromInteger $ natVal (Proxy :: Proxy n))
{-# INLINE replicateM #-}
