{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

#if HAVE_SIMD
# include "config.h"
#endif

import Control.Monad.ST (ST)
#if HAVE_SIMD
import Data.Maybe (catMaybes)
#endif
import Data.Word (Word8)

#if HAVE_SIMD
import Foreign.C (CSize(..))
import Foreign.Ptr (Ptr)
#endif

import Criterion.Main

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable as SV

#if HAVE_SIMD
import qualified Data.Vector.Generic.Sized as S
import Data.ReedSolomon (SIMDInstructions(..))
#endif
import qualified Data.ReedSolomon as RS
import qualified Data.ReedSolomon.Galois.NoAsm as NoAsm
#if HAVE_SIMD
import qualified Data.ReedSolomon.Galois.SIMD as SIMD
#endif

main :: IO ()
main = do
    level <- RS.simdInstructions
    putStrLn $ "Native instructions: " ++ maybe "None" show level

#if HAVE_SIMD
    let dependOn l prop = maybe Nothing (\lvl -> if l <= lvl then Just prop else Nothing ) level
#endif

    defaultMain [
        bgroup "Galois/galMulSlice/1048576" [
            bench "NoAsm" $ whnf (benchGalMulSlice NoAsm.galMulSlice 177) v1048576
#if HAVE_SIMD
          , bench "Native" $ whnf (benchGalMulSlice SIMD.galMulSlice 177) v1048576
#endif
          ]
#if HAVE_SIMD
      , bgroup "reedsolomon_gal_mul" $ catMaybes [
            Just $ bench "Native" $ whnf (benchRGM c_reedsolomon_gal_mul) v1048576
#if RS_HAVE_AVX2
          , dependOn AVX2 $ bench "AVX2" $ whnf (benchRGM c_reedsolomon_gal_mul_avx2) v1048576
#endif
#if RS_HAVE_AVX
          , dependOn AVX $ bench "AVX" $ whnf (benchRGM c_reedsolomon_gal_mul_avx) v1048576
#endif
#if RS_HAVE_SSSE3
          , dependOn SSSE3 $ bench "SSSE3" $ whnf (benchRGM c_reedsolomon_gal_mul_ssse3) v1048576
#endif
#if RS_HAVE_SSE2
          , dependOn SSE2 $ bench "SSE2" $ whnf (benchRGM c_reedsolomon_gal_mul_sse2) v1048576
#endif
#if RS_HAVE_GENERIC
          , Just $ bench "Generic" $ whnf (benchRGM c_reedsolomon_gal_mul_generic) v1048576
#endif
#if RS_HAVE_NEON
          , Just $ bench "NEON" $ whnf (benchRGM c_reedsolomon_gal_mul_neon) v1048576
#endif
#if RS_HAVE_ALTIVEC
          , Just $ bench "AltiVec" $ whnf (benchRGM c_reedsolomon_gal_mul_altivec) v1048576
#endif
          ]
      , bgroup "memcpy" [
            bench "memcpy" $ whnf (benchRGM memcpyAsCProto) v1048576
          ]
#endif
      ]
  where
    v1048576 = V.fromListN 1048576 $ cycle [minBound .. maxBound]
    benchGalMulSlice :: (forall s. Word8 -> SV.Vector Word8 -> SV.MVector s Word8 -> ST s ())
                     -> Word8
                     -> SV.Vector Word8
                     -> SV.Vector Word8
    benchGalMulSlice f c in_ = V.create $ do
        out <- MV.new (V.length in_)
        f c in_ out
        return out
#if HAVE_SIMD
    benchRGM :: SIMD.CProto
             -> SV.Vector Word8
             -> SV.Vector Word8
    benchRGM f in_ = V.create $ do
        out <- MV.new (V.length in_)
        _ <- f' v16 v16 in_ out
        return out
      where
        f' :: forall s.
              S.SVector 16 Word8
           -> S.SVector 16 Word8
           -> SV.Vector Word8
           -> SV.MVector s Word8
           -> ST s CSize
        f' = SIMD.cProtoToPrim f
        v16 = [0 .. 15]
#endif

#if HAVE_SIMD
type CProto = SIMD.CProto
foreign import ccall unsafe "reedsolomon_gal_mul" c_reedsolomon_gal_mul :: CProto
#if RS_HAVE_AVX2
foreign import ccall unsafe "reedsolomon_gal_mul_avx2" c_reedsolomon_gal_mul_avx2 :: CProto
#endif
#if RS_HAVE_AVX
foreign import ccall unsafe "reedsolomon_gal_mul_avx" c_reedsolomon_gal_mul_avx :: CProto
#endif
#if RS_HAVE_SSSE3
foreign import ccall unsafe "reedsolomon_gal_mul_ssse3" c_reedsolomon_gal_mul_ssse3 :: CProto
#endif
#if RS_HAVE_SSE2
foreign import ccall unsafe "reedsolomon_gal_mul_sse2" c_reedsolomon_gal_mul_sse2 :: CProto
#endif
#if RS_HAVE_GENERIC
foreign import ccall unsafe "reedsolomon_gal_mul_generic" c_reedsolomon_gal_mul_generic :: CProto
#endif
#if RS_HAVE_NEON
foreign import ccall unsafe "reedsolomon_gal_mul_neon" c_reedsolomon_gal_mul_neon :: CProto
#endif
#if RS_HAVE_ALTIVEC
foreign import ccall unsafe "reedsolomon_gal_mul_altivec" c_reedsolomon_gal_mul_altivec :: CProto
#endif

foreign import ccall unsafe "memcpy" c_memcpy :: Ptr a -> Ptr a -> CSize -> IO ()

memcpyAsCProto :: CProto
memcpyAsCProto _ _ in_ out len = do
    c_memcpy out in_ len
    return len
#endif
