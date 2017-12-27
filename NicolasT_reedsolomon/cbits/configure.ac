#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

dnl Assumed minimal alignment of any input/output buffers allocated by GHC
m4_define([_RS_ASSUMED_ALIGNMENT], [16])

AC_PREREQ([2.68])
AC_INIT([reedsolomon],
        m4_esyscmd_s([grep ^Version ../reedsolomon.cabal | sed -e 's/^Version\:[[:space:]]*//']),
        [https://github.com/NicolasT/reedsolomon/issues],
        [reedsolomon],
        [https://github.com/NicolasT/reedsolomon])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([reedsolomon.h])
AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([1.11 foreign -Wall -Werror])
AM_MAINTAINER_MODE([enable])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_CANONICAL_BUILD
AC_CANONICAL_HOST

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CC_C99
AC_PROG_CPP
AC_PROG_AWK
AC_PROG_SED
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AC_PROG_RANLIB

# Checks for libraries.

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT8_T
AC_TYPE_UINT64_T

## Checks for supported function attributes.
# Work-around compiler emitting warnings for unrecognized flags added to CFLAGS
# before, which causes the attribute checks to fail.
rs_gcc_func_attribute_save_flags=$[]_AC_LANG_PREFIX[]FLAGS
_AC_LANG_PREFIX[]FLAGS=""
AX_GCC_FUNC_ATTRIBUTE([always_inline])
AX_GCC_FUNC_ATTRIBUTE([const])
AX_GCC_FUNC_ATTRIBUTE([hot])
AX_GCC_FUNC_ATTRIBUTE([ifunc])
AX_GCC_FUNC_ATTRIBUTE([force_align_arg_pointer])
_AC_LANG_PREFIX[]FLAGS=$rs_gcc_func_attribute_save_flags

## Checks for supported generic compiler options.
AX_APPEND_COMPILE_FLAGS([-Werror=unknown-pragmas -Werror=unused-command-line-argument -Werror=unknown-warning-option])
AX_APPEND_COMPILE_FLAGS([-ggdb3])
AS_IF([test x$PACKAGE_VERSION = x999],
      AX_APPEND_COMPILE_FLAGS([-Werror]))
AX_APPEND_COMPILE_FLAGS([-Wall -Wextra -pedantic -Wno-pedantic-ms-format -Wundef -Wunknown-pragmas])
AX_APPEND_COMPILE_FLAGS([-O3])
dnl Handle "sorry, unimplemented: Graphite loop optimizations can only be used if the libcloog-ppl0 package is installed"
dnl The error above is only triggered when the Graphite optimization is actually
dnl used, i.e. not for the 'default' program source compiled to determine
dnl compiler support for some flag. The snippet below should trigger it though.
dnl See also: https://lists.debian.org/debian-gcc/2015/06/msg00164.html
AX_APPEND_COMPILE_FLAGS([-floop-parallelize-all], [], [], [
    AC_LANG_PROGRAM([[
            extern int argc;
            extern char **argv;
        ]], [[
            int x = 0, result = 0;
            for(x = 0; x < argc; x++) {
                result |= (int)argv[x][0];
            }
            return result;
        ]])])
AX_APPEND_COMPILE_FLAGS([-funroll-loops -ftree-vectorize -fprefetch-loop-arrays])
AX_APPEND_COMPILE_FLAGS([-frecord-gcc-switches])

## Check for -fPIC
AC_MSG_CHECKING([whether C compiler accepts -fPIC])
rs_gcc_fpic_save_flags=$[]_AC_LANG_PREFIX[]FLAGS
_AC_LANG_PREFIX[]FLAGS="-fPIC -Werror"
AC_COMPILE_IFELSE(
    [AC_LANG_PROGRAM(
        [],
        [return 0;])],
    [AC_MSG_RESULT([yes])
     _AC_LANG_PREFIX[]FLAGS="$rs_gcc_fpic_save_flags -fPIC"],
    [AC_MSG_RESULT([no])
     _AC_LANG_PREFIX[]FLAGS="$rs_gcc_fpic_save_flags"])

## Checks for supported ISA compiler options.
AX_CHECK_COMPILE_FLAG([-msse2], [rs_sse2=1], [rs_sse2=0])
AX_CHECK_COMPILE_FLAG([-mssse3], [rs_ssse3=1], [rs_ssse3=0])
AX_CHECK_COMPILE_FLAG([-mavx], [rs_avx=1], [rs_avx=0])
AX_CHECK_COMPILE_FLAG([-mavx2], [rs_avx2=1], [rs_avx2=0])

## Checks for compiler-specific features
AC_MSG_CHECKING([whether C compiler supports __builtin_shuffle])
AC_RUN_IFELSE(
    [AC_LANG_PROGRAM(
        [
            #include <stdint.h>

            typedef uint8_t v16u8v __attribute__((vector_size(16)));
        ],
        [[
            v16u8v v1 = { 0, 1,  2,  3,  4,  5,  6,  7
                        , 8, 9, 10, 11, 12, 13, 14, 15 },
                   v2 = { 0, 0, 0, 0, 0, 0, 0, 0
                        , 0, 0, 0, 0, 0, 0, 0, 0 },
                   mask = {  0, 20,  4, 24,  8, 28, 12, 32
                          , 18,  2, 22,  6, 26, 10, 30, 16 },
                   result = __builtin_shuffle(v1, v2, mask),
                   expected = { 0, 0, 4, 0, 8,  0, 12, 0
                              , 0, 2, 0, 6, 0, 10,  0, 0 };

            int equal =    result[0] == expected[0]
                        && result[1] == expected[1]
                        && result[2] == expected[2]
                        && result[3] == expected[3]
                        && result[4] == expected[4]
                        && result[5] == expected[5]
                        && result[6] == expected[6]
                        && result[7] == expected[7]
                        && result[8] == expected[8]
                        && result[9] == expected[9]
                        && result[10] == expected[10]
                        && result[11] == expected[11]
                        && result[12] == expected[12]
                        && result[13] == expected[13]
                        && result[14] == expected[14]
                        && result[15] == expected[15];

            return equal ? 0 : 1;
        ]])],
    [AC_MSG_RESULT([yes])
     rs_have_builtin_shuffle=1],
    [AC_MSG_RESULT([no])
     rs_have_builtin_shuffle=0],
    [AC_MSG_RESULT([no (assumed, cross-compiling)])
     rs_have_builtin_shuffle=0])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_BUILTIN_SHUFFLE],
    [$rs_have_builtin_shuffle],
    [Define to 1 if C compiller supports __builtin_shuffle])

AC_MSG_CHECKING([whether Clang's `loop unroll` pragma works])
AC_COMPILE_IFELSE(
    [AC_LANG_PROGRAM(
        [
            #include <stdio.h>
        ],
        [
            int i = 0;
            #pragma clang loop unroll(enable)
            for(i = 0; i < 8; i++) {
                printf("Hello, world!");
            }
        ])],
    [AC_MSG_RESULT([yes])
     rs_have_clang_loop_unroll=1],
    [AC_MSG_RESULT([no])
     rs_have_clang_loop_unroll=0])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_CLANG_LOOP_UNROLL],
    [$rs_have_clang_loop_unroll],
    [Define to 1 if Clang's `loop unroll` pragma works])

# Checks for header files.
AC_CHECK_HEADERS([stddef.h stdint.h unistd.h])
AC_CHECK_HEADERS([cpuid.h])

## Checks for ISA-specific header files.
rs_simd_headers_save_flags=$CPPFLAGS
if test x$rs_sse2 = x1; then
    CPPFLAGS="$CPPFLAGS -msse2"
fi
if test x$rs_ssse3 = x1; then
    CPPFLAGS="$CPPFLAGS -mssse3"
fi
if test x$rs_avx = x1; then
    CPPFLAGS="$CPPFLAGS -mavx"
fi
if test x$rs_avx2 = x1; then
    CPPFLAGS="$CPPFLAGS -mavx2"
fi
AC_CHECK_HEADERS([emmintrin.h])
AC_CHECK_HEADERS([tmmintrin.h])
AC_CHECK_HEADERS([immintrin.h])
CPPFLAGS=$rs_simd_headers_save_flags

AC_CHECK_HEADERS([arm_neon.h], [rs_neon=1], [rs_neon=0])
# Figure out whether we have vqtbl1q_u8 support
AC_CACHE_CHECK([whether vqtbl1q_u8 works], [rs_cv_vqtbl1q_u8_works],
    [AC_COMPILE_IFELSE([
        AC_LANG_PROGRAM([[
                #include <arm_neon.h>
            ]], [[
                const uint8x16_t tab = vdupq_n_u8(1),
                                 idx = vdupq_n_u8(2),
                                 result = vqtbl1q_u8(tab, idx);

                (void)result;
            ]])],
            [rs_cv_vqtbl1q_u8_works=yes],
            [rs_cv_vqtbl1q_u8_works=no])])
if test x$rs_cv_vqtbl1q_u8_works = xyes; then
    rs_have_vqtbl1q_u8=1
else
    rs_have_vqtbl1q_u8=0
fi
AC_DEFINE_UNQUOTED(
    [RS_HAVE_VQTBL1Q_U8],
    [$rs_have_vqtbl1q_u8],
    [Define to 1 if vqtbl1q_u8 works])

AC_CHECK_HEADERS([altivec.h], [rs_altivec=1], [rs_altivec=0])
# Figure out whether we have POWER8 support
AC_CACHE_CHECK([whether vec_vsrd works], [rs_cv_vec_vsrd_works],
    [AC_COMPILE_IFELSE([
        AC_LANG_PROGRAM([[
                #include <altivec.h>
            ]], [[
                __vector unsigned long v0 = { 0, 1 },
                                       v1 = { 2, 3 },
                                       v2 = vec_vsrd(v0, v1);
                (void)v2;
            ]])],
            [rs_cv_vec_vsrd_works=yes],
            [rs_cv_vec_vsrd_works=no])])

if test x$rs_cv_vec_vsrd_works = xyes; then
    rs_have_vec_vsrd=1
else
    rs_have_vec_vsrd=0
fi
AC_DEFINE_UNQUOTED(
    [RS_HAVE_VEC_VSRD],
    [$rs_have_vec_vsrd],
    [Define to 1 if vec_vsrd works])

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_DECLS(
    [[__get_cpuid_count(const unsigned int, const unsigned int, unsigned int *, unsigned int *, unsigned int *, unsigned int *)]],
    [],
    [],
    [[
        #if defined(HAVE_CPUID_H) && HAVE_CPUID_H
        # include <cpuid.h>
        #endif
    ]])

# Definitions based on discovered compiler or ISA characteristics.
rs_generic=1
if test x$rs_neon = x1; then
    # When building with ARM NEON support, we don't build generic
    rs_generic=0
fi
if test x$rs_altivec = x1; then
    # When building with PPC AltiVec support, we don't build generic
    rs_generic=0
fi
AM_CONDITIONAL(RS_HAVE_GENERIC, [test x$rs_generic = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_GENERIC],
    [$rs_generic],
    [Define to 1 if the generic routine works])
AM_CONDITIONAL(RS_HAVE_SSE2, [test x$rs_sse2 = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_SSE2],
    [$rs_sse2],
    [Define to 1 if AVX2 works])
AM_CONDITIONAL(RS_HAVE_SSSE3, [test x$rs_ssse3 = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_SSSE3],
    [$rs_ssse3],
    [Define to 1 if SSSE3 works])
AM_CONDITIONAL(RS_HAVE_AVX, [test x$rs_avx = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_AVX],
    [$rs_avx],
    [Define to 1 if AVX works])
AM_CONDITIONAL(RS_HAVE_AVX2, [test x$rs_avx2 = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_AVX2],
    [$rs_avx2],
    [Define to 1 if AVX2 works])
AM_CONDITIONAL(RS_HAVE_NEON, [test x$rs_neon = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_NEON],
    [$rs_neon],
    [Define to 1 if NEON works])
AM_CONDITIONAL(RS_HAVE_ALTIVEC, [test x$rs_altivec = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_ALTIVEC],
    [$rs_altivec],
    [Define to 1 if AltiVec works])

AC_MSG_CHECKING([for assumed data alignment])
AC_MSG_RESULT(_RS_ASSUMED_ALIGNMENT)
AC_DEFINE_UNQUOTED(
    [RS_ASSUMED_ALIGNMENT],
    _RS_ASSUMED_ALIGNMENT,
    [Assumed data alignment])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

rs_backends=""
if test x$rs_generic = x1; then
    if test x$rs_have_builtin_shuffle = x1; then
        rs_backends="$rs_backends, generic (__builtin_shuffle)"
    else
        rs_backends="$rs_backends, generic"
    fi
fi
if test x$rs_sse2 = x1; then
    if test x$rs_have_builtin_shuffle = x1; then
        rs_backends="$rs_backends, sse2 (__builtin_shuffle)"
    else
        rs_backends="$rs_backends, sse2"
    fi
fi
if test x$rs_ssse3 = x1; then
    rs_backends="$rs_backends, ssse3"
fi
if test x$rs_avx = x1; then
    rs_backends="$rs_backends, avx"
fi
if test x$rs_avx2 = x1; then
    rs_backends="$rs_backends, avx2"
fi
if test x$rs_neon = x1; then
    if test x$rs_have_vqtbl1q_u8 = x1; then
        rs_backends="$rs_backends, neon (AARCH64)"
    else
        rs_backends="$rs_backends, neon"
    fi
fi
if test x$rs_altivec = x1; then
    if test x$rs_have_vec_vsrd = x1; then
        rs_backends="$rs_backends, altivec (POWER8)"
    else
        rs_backends="$rs_backends, altivec"
    fi
fi

rs_backends=`echo $rs_backends | $SED s/^,\ //`

echo ["
----------------------------------------------------------------------
Configure completed successfully.

    Version: $PACKAGE_VERSION
    Compiler: $CC

    Build platform: $build
    Host platform: $host
    Enabled backends: $rs_backends
"]

case $host_cpu in
    arm*)
        if test x$rs_neon = x0; then
            echo -e "    WARNING: Not using ARM NEON, performance will be degraded.\n"
        fi;;
    i[3456]86*|x86_64*)
        # Assume a compiler which supports AVX/AVX2 also supports SSSE3
        if test x$rs_ssse3 = x0; then
            echo -e "    WARNING: Not using Intel SSSE3, performance will be degraded.\n"
        fi;;
    powerpc*)
        if test x$rs_altivec = x0; then
            echo -e "    WARNING: Not using PowerPC AltiVec, performance will be degraded.\n"
        fi;;
esac
