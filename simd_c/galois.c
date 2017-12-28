#include<stdio.h>
#include<stdlib.h>

#define V16UC_SIZE 16

// For targets arch : x86_64, aarch64
typedef unsigned char v16uc __attribute__ ((vector_size (V16UC_SIZE), aligned (1)));

const v16uc low_mask   = {0xf, 0xf, 0xf, 0xf,
                          0xf, 0xf, 0xf, 0xf,
                          0xf, 0xf, 0xf, 0xf,
                          0xf, 0xf, 0xf, 0xf};

const v16uc high_shift = {4, 4, 4, 4,
                          4, 4, 4, 4,
                          4, 4, 4, 4,
                          4, 4, 4, 4};

void simd_v16uc_mul_slice(const unsigned char* low,
                          const unsigned char* high,
                          const unsigned char* input,
                          unsigned char*       out) {
  size_t i = 0;
  if (len > 16 && vect_yes) {
    size_t len_minus_16 = len - V16UC_SIZE;
    v16uc* in_v  = (v16uc *) input;
    v16uc* out_v = (v16uc *) out;
    v16uc  in_low, in_high;
    v16uc  mul_low, mul_high;
    for (; i < len_minus_16; i += V16UC_SIZE, in_v++, out_v++) {
      in_low  = *in_v &  low_mask;
      in_high = *in_v >> high_shift;

      #ifdef __GNUC__
        mul_low = __builtin_shuffle(low, in_low);
      #elif  __clang__
        *out = __builtin_shufflevector();
      #endif
    }
  }
  unsigned char l, r;
  for (; i < len; i++) {
    l = input[i] & 0xf;
    h = input[i] >> 4;
    out[i] = low[l] ^ high[h];
  }
}
