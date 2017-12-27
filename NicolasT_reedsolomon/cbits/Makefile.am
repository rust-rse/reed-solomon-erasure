ACLOCAL_AMFLAGS = -I m4

libreedsolomon_a_SOURCES = reedsolomon_dispatch.c \
		           reedsolomon.h
noinst_LIBRARIES = libreedsolomon.a

SIMD_SRC = reedsolomon.c \
	   reedsolomon.h

if RS_HAVE_GENERIC
libreedsolomon_generic_a_SOURCES = $(SIMD_SRC)
libreedsolomon_generic_a_CPPFLAGS = -DTARGET=generic
noinst_LIBRARIES += libreedsolomon-generic.a
endif

if RS_HAVE_SSE2
libreedsolomon_sse2_a_SOURCES = $(SIMD_SRC)
libreedsolomon_sse2_a_CPPFLAGS = -DTARGET=sse2
libreedsolomon_sse2_a_CFLAGS = -msse2 $(AM_CFLAGS)
noinst_LIBRARIES += libreedsolomon-sse2.a
endif

if RS_HAVE_SSSE3
libreedsolomon_ssse3_a_SOURCES = $(SIMD_SRC)
libreedsolomon_ssse3_a_CPPFLAGS = -DTARGET=ssse3
libreedsolomon_ssse3_a_CFLAGS = -mssse3 $(AM_CFLAGS)
noinst_LIBRARIES += libreedsolomon-ssse3.a
endif

if RS_HAVE_AVX
libreedsolomon_avx_a_SOURCES = $(SIMD_SRC)
libreedsolomon_avx_a_CPPFLAGS = -DTARGET=avx -DHOT
libreedsolomon_avx_a_CFLAGS = -mavx $(AM_CFLAGS)
noinst_LIBRARIES += libreedsolomon-avx.a
endif

if RS_HAVE_AVX2
libreedsolomon_avx2_a_SOURCES = $(SIMD_SRC)
libreedsolomon_avx2_a_CPPFLAGS = -DTARGET=avx2 -DHOT
libreedsolomon_avx2_a_CFLAGS = -mavx2 $(AM_CFLAGS)
noinst_LIBRARIES += libreedsolomon-avx2.a
endif

if RS_HAVE_NEON
libreedsolomon_neon_a_SOURCES = $(SIMD_SRC)
libreedsolomon_neon_a_CPPFLAGS = -DTARGET=neon -DHOT
noinst_LIBRARIES += libreedsolomon-neon.a
endif

if RS_HAVE_ALTIVEC
libreedsolomon_altivec_a_SOURCES = $(SIMD_SRC)
libreedsolomon_altivec_a_CPPFLAGS = -DTARGET=altivec -DHOT
noinst_LIBRARIES += libreedsolomon-altivec.a
endif

noinst_PROGRAMS = reedsolomon-gal-mul-stdio
reedsolomon_gal_mul_stdio_LDADD = libreedsolomon.a \
				  $(LIBOBJS)
if RS_HAVE_GENERIC
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-generic.a
endif
if RS_HAVE_SSE2
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-sse2.a
endif
if RS_HAVE_SSSE3
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-ssse3.a
endif
if RS_HAVE_AVX
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-avx.a
endif
if RS_HAVE_AVX2
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-avx2.a
endif
if RS_HAVE_NEON
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-neon.a
endif
if RS_HAVE_ALTIVEC
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-altivec.a
endif

dist_noinst_SCRIPTS = compare-isa.sh
EXTRA_DIST = reedsolomon-gal-mul-stdio-quickcheck.hs
