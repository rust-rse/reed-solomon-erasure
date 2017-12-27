module Data.ReedSolomon.Backend (
      Backend(..)
    ) where

import Data.Word (Word8)

import Control.Monad.ST (ST)

import Data.Vector.Storable as SV (Vector, MVector)

data Backend s = Backend { backendName :: String
                         , backendGalMulSlice :: Word8
                                              -> SV.Vector Word8
                                              -> SV.MVector s Word8
                                              -> ST s ()
                         , backendGalMulSliceXor :: Word8
                                                 -> SV.Vector Word8
                                                 -> SV.MVector s Word8
                                                 -> ST s ()
                         }
