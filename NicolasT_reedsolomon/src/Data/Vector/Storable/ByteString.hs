{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Data.Vector.Storable.ByteString
-- Description : Conversion routines between 'ByteString's and 'Vector's of bytes
-- Copyright   : (C) 2015 Nicolas Trangez
-- License     : MIT (see the file LICENSE)
-- Maintainer  : Nicolas Trangez <ikke@nicolast.be>
-- Stability   : provisional
--
-- This module provides some conversion routines between 'ByteString's and
-- 'Vector's of 'Word8' bytes, as used throughout the library.
--
-- It also provides a 'Control.Lens'-style isomorphism.

module Data.Vector.Storable.ByteString (
    -- * Conversion routines
      fromByteString
    , toByteString
    -- * Isomorphism
    , Iso, Iso', iso
    ) where

import Data.Word (Word8)

import Data.Profunctor (Profunctor, dimap)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as I

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S

-- | Convert a 'ByteString' into a 'Vector' of bytes.
--
-- No data is copied, so in-place mutations break referential transparency.
--
-- O(1).
fromByteString :: ByteString -> Vector Word8
fromByteString = unsafeFromForeignPtr . I.toForeignPtr
  where
    unsafeFromForeignPtr (p, o, l) = S.unsafeFromForeignPtr p o l
{-# INLINE fromByteString #-}

-- | Convert a 'Vector' of bytes into a 'ByteString'.
--
-- No data is copied, so in-place mutations break referential transparency.
--
-- O(1).
toByteString :: Vector Word8 -> ByteString
toByteString = fromForeignPtr . S.unsafeToForeignPtr
  where
    fromForeignPtr (p, o, l) = I.fromForeignPtr p o l
{-# INLINE toByteString #-}

-- | The type of an isomorphism a la 'Control.Lens.Iso'.
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
-- | A 'Control.Lens.Type.Simple' 'Iso'.
type Iso' s a = Iso s s a a

-- | An 'Iso' between a 'ByteString' and a 'Vector' of bytes.
iso :: Iso' ByteString (Vector Word8)
iso = dimap fromByteString (fmap toByteString)
