> {-# LANGUAGE DataKinds #-}

> module Data.ReedSolomon.Galois.SIMD (
>       backend
>     , galMulSlice
>     , galMulSliceXor
>     , CProto
>     , cProtoToPrim
>     ) where
>
> import Control.Monad (when)
> import Data.Bits (xor)
> import Data.Word (Word8)
> import Foreign.C (CSize(..))
> import Foreign.Ptr (Ptr)
>
> import Control.Monad.Primitive (PrimMonad, PrimState, unsafePrimToPrim)
>
> import Control.Loop (numLoop)
>
> import qualified Data.Vector.Generic as V (unsafeIndex)
> import qualified Data.Vector.Storable as V hiding (unsafeIndex)
> import qualified Data.Vector.Storable.Mutable as MV
>
> import qualified Data.Vector.Generic.Sized as S
> import Data.ReedSolomon.Backend (Backend(..))
> import qualified Data.ReedSolomon.Galois.GenTables as GenTables

//+build !noasm
//+build !appengine

// Copyright 2015, Klaus Post, see LICENSE for details.

package reedsolomon

import (
	"github.com/klauspost/cpuid"
)

func galMulSSSE3(low, high, in, out []byte)
func galMulSSSE3Xor(low, high, in, out []byte)

// This is what the assembler rountes does in blocks of 16 bytes:
/*
func galMulSSSE3(low, high, in, out []byte) {
	for n, input := range in {
		l := input & 0xf
		h := input >> 4
		out[n] = low[l] ^ high[h]
	}
}

func galMulSSSE3Xor(low, high, in, out []byte) {
	for n, input := range in {
		l := input & 0xf
		h := input >> 4
		out[n] ^= low[l] ^ high[h]
	}
}
*/

func galMulSlice(c byte, in, out []byte) {
	var done int
	if cpuid.CPU.SSSE3() {
		galMulSSSE3(mulTableLow[c][:], mulTableHigh[c][:], in, out)
		done = (len(in) >> 4) << 4
	}
	remain := len(in) - done
	if remain > 0 {
		mt := mulTable[c]
		for i := done; i < len(in); i++ {
			out[i] = mt[in[i]]
		}
	}
}

> galMulSlice :: (Functor m, PrimMonad m) => Word8 -> V.Vector Word8 -> V.MVector (PrimState m) Word8 -> m ()
> galMulSlice c in_ out = do
>     let mtlc = S.index GenTables.mulTableLow c
>         mthc = S.index GenTables.mulTableHigh c
>         len = V.length in_
>     done <- fromIntegral `fmap` galMulSSSE3 mtlc mthc in_ out
>     when (done /= len) $ do
>         let mt = S.index GenTables.mulTable c
>         numLoop done (len - 1) $ \i ->
>             MV.unsafeWrite out i (S.index mt (V.unsafeIndex in_ i))
>   where
>     galMulSSSE3 = cProtoToPrim c_reedsolomon_gal_mul
> {-# INLINE galMulSlice #-}

func galMulSliceXor(c byte, in, out []byte) {
	var done int
	if cpuid.CPU.SSSE3() {
		galMulSSSE3Xor(mulTableLow[c][:], mulTableHigh[c][:], in, out)
		done = (len(in) >> 4) << 4
	}
	remain := len(in) - done
	if remain > 0 {
		mt := mulTable[c]
		for i := done; i < len(in); i++ {
			out[i] ^= mt[in[i]]
		}
	}
}

> galMulSliceXor :: (Functor m, PrimMonad m) => Word8 -> V.Vector Word8 -> V.MVector (PrimState m) Word8 -> m ()
> galMulSliceXor c in_ out = do
>     let mtlc = S.index GenTables.mulTableLow c
>         mthc = S.index GenTables.mulTableHigh c
>         len = min (V.length in_) (MV.length out)
>     done <- fromIntegral `fmap` galMulSSSE3Xor mtlc mthc in_ out
>     when (done /= len) $ do
>         let mt = S.index GenTables.mulTable c
>         numLoop done (len - 1) $ \i -> do
>             r <- MV.unsafeRead out i
>             let m = S.index mt (V.unsafeIndex in_ i)
>                 r' = r `xor` m
>             MV.unsafeWrite out i r'
>   where
>     galMulSSSE3Xor = cProtoToPrim c_reedsolomon_gal_mul_xor
> {-# INLINE galMulSliceXor #-}

> type CProto = Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> CSize -> IO CSize
>
> cProtoToPrim :: PrimMonad m
>              => CProto
>              -> S.SVector 16 Word8
>              -> S.SVector 16 Word8
>              -> V.Vector Word8
>              -> V.MVector (PrimState m) Word8
>              -> m CSize
> cProtoToPrim inner low high in_ out@(MV.MVector ol op) = do
>     let len = fromIntegral $ min (V.length in_) (MV.length out)
>     unsafePrimToPrim $ -- TODO Safe?
>         S.unsafeWith low $ \low' ->
>         S.unsafeWith high $ \high' ->
>         V.unsafeWith in_ $ \in_' ->
>         MV.unsafeWith (MV.MVector ol op) $ \out' ->
>         inner low' high' in_' out' len
> {-# INLINE cProtoToPrim #-}

> foreign import ccall unsafe "reedsolomon_gal_mul" c_reedsolomon_gal_mul :: CProto
> foreign import ccall unsafe "reedsolomon_gal_mul_xor" c_reedsolomon_gal_mul_xor :: CProto

> backend :: Backend s
> backend = Backend { backendName = "SIMD"
>                   , backendGalMulSlice = galMulSlice
>                   , backendGalMulSliceXor = galMulSliceXor
>                   }
