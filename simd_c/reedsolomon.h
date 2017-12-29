/* reedsolomon.h - SIMD-optimized Galois-field multiplication routines
 *
 * Copyright (c) 2015, 2016 Nicolas Trangez
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE
 */

#include <unistd.h>
#include <stdint.h>

#if HAVE_CONFIG_H
# include "config.h"
#endif

#define PROTO_RETURN size_t
#define PROTO_ARGS                              \
        const uint8_t low[16],                  \
        const uint8_t high[16],                 \
        const uint8_t *restrict const in,       \
        uint8_t *restrict const out,            \
        const size_t len
#define PROTO(name)                     \
        PROTO_RETURN                    \
        __attribute__((nonnull))        \
        name (PROTO_ARGS)

#if RS_HAVE_AVX2
PROTO(reedsolomon_gal_mul_avx2);
PROTO(reedsolomon_gal_mul_xor_avx2);
#endif
#if RS_HAVE_AVX
PROTO(reedsolomon_gal_mul_avx);
PROTO(reedsolomon_gal_mul_xor_avx);
#endif
#if RS_HAVE_SSSE3
PROTO(reedsolomon_gal_mul_ssse3);
PROTO(reedsolomon_gal_mul_xor_ssse3);
#endif
#if RS_HAVE_SSE2
PROTO(reedsolomon_gal_mul_sse2);
PROTO(reedsolomon_gal_mul_xor_sse2);
#endif
#if RS_HAVE_GENERIC
PROTO(reedsolomon_gal_mul_generic);
PROTO(reedsolomon_gal_mul_xor_generic);
#endif
#if RS_HAVE_NEON
PROTO(reedsolomon_gal_mul_neon);
PROTO(reedsolomon_gal_mul_xor_neon);
#endif
#if RS_HAVE_ALTIVEC
PROTO(reedsolomon_gal_mul_altivec);
PROTO(reedsolomon_gal_mul_xor_altivec);
#endif

PROTO(reedsolomon_gal_mul);
PROTO(reedsolomon_gal_mul_xor);

typedef enum {
        REEDSOLOMON_CPU_GENERIC = 0,
        REEDSOLOMON_CPU_SSE2 = 1,
        REEDSOLOMON_CPU_SSSE3 = 2,
        REEDSOLOMON_CPU_AVX = 3,
        REEDSOLOMON_CPU_AVX2 = 4,
        REEDSOLOMON_CPU_NEON = 5,
        REEDSOLOMON_CPU_ALTIVEC = 6,
} reedsolomon_cpu_support;

reedsolomon_cpu_support reedsolomon_determine_cpu_support(void);
