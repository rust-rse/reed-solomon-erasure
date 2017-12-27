{-# LANGUAGE CPP #-}
-- |
-- Module : Data.Vector.Generic.Compat
-- Description : Vector compatibility functions
-- Copyright : (C) 2015 Nicolas Trangez
-- License : MIT (see the file LICENSE)
-- Maintainer : Nicolas Trangez <ikke@nicolast.be>
-- Stability : experimental
--
-- This module implements several utility functions related to 'Vector',
-- backported from newer versions, used in the 'reedsolomon' library.

module Data.Vector.Generic.Compat (
      iforM_
    ) where

#if !MIN_VERSION_vector(0, 11, 0)
import qualified Data.Vector.Fusion.Stream as S
#endif
import qualified Data.Vector.Generic as V

-- | Monadic forgetful loop over a 'Vector' with supplied index.
iforM_ :: (Monad m, V.Vector v a)
       => v a -- ^ Input 'Vector'
       -> (Int -> a -> m ()) -- ^ Action to apply
       -> m ()
#if MIN_VERSION_vector(0, 11, 0)
iforM_ = flip V.imapM_
#else
iforM_ as f = S.mapM_ (uncurry f) $ S.indexed $ V.stream as
#endif
{-# INLINE iforM_ #-}
