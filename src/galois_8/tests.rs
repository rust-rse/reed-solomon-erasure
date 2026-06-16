extern crate alloc;

use alloc::vec;

#[cfg(feature = "std")]
use super::profile::{RS_NEON_MUL_SLICE_XOR_SCHEDULE_ENV, parse_rust_neon_xor_unroll};
use super::*;
use crate::tests::fill_random;
use rand;

#[cfg(all(
    feature = "std",
    feature = "simd-neon",
    target_arch = "aarch64",
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios"))
))]
static NEON_PROFILE_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

#[cfg(feature = "std")]
fn with_env_var<R>(key: &str, value: &str, f: impl FnOnce() -> R) -> R {
    // SAFETY: test-only scoped env var override, restored immediately after use.
    unsafe {
        std::env::set_var(key, value);
    }
    let result = f();
    // SAFETY: paired cleanup for the scoped env var override above.
    unsafe {
        std::env::remove_var(key);
    }
    result
}

static BACKBLAZE_LOG_TABLE: [u8; 256] = [
    //-1,    0,    1,   25,    2,   50,   26,  198,
    // first value is changed from -1 to 0
    0, 0, 1, 25, 2, 50, 26, 198, 3, 223, 51, 238, 27, 104, 199, 75, 4, 100, 224, 14, 52, 141, 239,
    129, 28, 193, 105, 248, 200, 8, 76, 113, 5, 138, 101, 47, 225, 36, 15, 33, 53, 147, 142, 218,
    240, 18, 130, 69, 29, 181, 194, 125, 106, 39, 249, 185, 201, 154, 9, 120, 77, 228, 114, 166, 6,
    191, 139, 98, 102, 221, 48, 253, 226, 152, 37, 179, 16, 145, 34, 136, 54, 208, 148, 206, 143,
    150, 219, 189, 241, 210, 19, 92, 131, 56, 70, 64, 30, 66, 182, 163, 195, 72, 126, 110, 107, 58,
    40, 84, 250, 133, 186, 61, 202, 94, 155, 159, 10, 21, 121, 43, 78, 212, 229, 172, 115, 243,
    167, 87, 7, 112, 192, 247, 140, 128, 99, 13, 103, 74, 222, 237, 49, 197, 254, 24, 227, 165,
    153, 119, 38, 184, 180, 124, 17, 68, 146, 217, 35, 32, 137, 46, 55, 63, 209, 91, 149, 188, 207,
    205, 144, 135, 151, 178, 220, 252, 190, 97, 242, 86, 211, 171, 20, 42, 93, 158, 132, 60, 57,
    83, 71, 109, 65, 162, 31, 45, 67, 216, 183, 123, 164, 118, 196, 23, 73, 236, 127, 12, 111, 246,
    108, 161, 59, 82, 41, 157, 85, 170, 251, 96, 134, 177, 187, 204, 62, 90, 203, 89, 95, 176, 156,
    169, 160, 81, 11, 245, 22, 235, 122, 117, 44, 215, 79, 174, 213, 233, 230, 231, 173, 232, 116,
    214, 244, 234, 168, 80, 88, 175,
];

const PROPERTY_TEST_VALUES: [u8; 16] = [
    0, 1, 2, 3, 7, 15, 31, 63, 85, 127, 128, 129, 170, 192, 254, 255,
];

#[test]
fn log_table_same_as_backblaze() {
    for i in 0..256 {
        assert_eq!(LOG_TABLE[i], BACKBLAZE_LOG_TABLE[i]);
    }
}

#[test]
fn test_associativity() {
    for a in 0..256 {
        let a = a as u8;
        for b in 0..256 {
            let b = b as u8;
            for &c in &PROPERTY_TEST_VALUES {
                let x = add(a, add(b, c));
                let y = add(add(a, b), c);
                assert_eq!(x, y);
                let x = mul(a, mul(b, c));
                let y = mul(mul(a, b), c);
                assert_eq!(x, y);
            }
        }
    }
}

quickcheck! {
    fn qc_add_associativity(a: u8, b: u8, c: u8) -> bool {
        add(a, add(b, c)) == add(add(a, b), c)
    }

    fn qc_mul_associativity(a: u8, b: u8, c: u8) -> bool {
        mul(a, mul(b, c)) == mul(mul(a, b), c)
    }
}

#[test]
fn test_identity() {
    for a in 0..256 {
        let a = a as u8;
        let b = sub(0, a);
        let c = sub(a, b);
        assert_eq!(c, 0);
        if a != 0 {
            let b = div(1, a);
            let c = mul(a, b);
            assert_eq!(c, 1);
        }
    }
}

quickcheck! {
    fn qc_additive_identity(a: u8) -> bool {
        sub(a, sub(0, a)) == 0
    }

    fn qc_multiplicative_identity(a: u8) -> bool {
        if a == 0 { true }
        else      { mul(a, div(1, a)) == 1 }
    }
}

#[test]
fn test_commutativity() {
    for a in 0..256 {
        let a = a as u8;
        for b in 0..256 {
            let b = b as u8;
            let x = add(a, b);
            let y = add(b, a);
            assert_eq!(x, y);
            let x = mul(a, b);
            let y = mul(b, a);
            assert_eq!(x, y);
        }
    }
}

quickcheck! {
    fn qc_add_commutativity(a: u8, b: u8) -> bool {
        add(a, b) == add(b, a)
    }

    fn qc_mul_commutativity(a: u8, b: u8) -> bool {
        mul(a, b) == mul(b, a)
    }
}

#[test]
fn test_distributivity() {
    for a in 0..256 {
        let a = a as u8;
        for b in 0..256 {
            let b = b as u8;
            for &c in &PROPERTY_TEST_VALUES {
                let x = mul(a, add(b, c));
                let y = add(mul(a, b), mul(a, c));
                assert_eq!(x, y);
            }
        }
    }
}

quickcheck! {
    fn qc_add_distributivity(a: u8, b: u8, c: u8) -> bool {
        mul(a, add(b, c)) == add(mul(a, b), mul(a, c))
    }
}

#[test]
fn test_exp() {
    for a in 0..256 {
        let a = a as u8;
        let mut power = 1u8;
        for j in 0..256 {
            let x = exp(a, j);
            assert_eq!(x, power);
            power = mul(power, a);
        }
    }
}

#[test]
fn test_galois() {
    assert_eq!(mul(3, 4), 12);
    assert_eq!(mul(7, 7), 21);
    assert_eq!(mul(23, 45), 41);

    let input = [
        0, 1, 2, 3, 4, 5, 6, 10, 50, 100, 150, 174, 201, 255, 99, 32, 67, 85, 200, 199, 198, 197,
        196, 195, 194, 193, 192, 191, 190, 189, 188, 187, 186, 185,
    ];
    let mut output1 = vec![0; input.len()];
    let mut output2 = vec![0; input.len()];
    mul_slice(25, &input, &mut output1);
    let expect = [
        0x0, 0x19, 0x32, 0x2b, 0x64, 0x7d, 0x56, 0xfa, 0xb8, 0x6d, 0xc7, 0x85, 0xc3, 0x1f, 0x22,
        0x7, 0x25, 0xfe, 0xda, 0x5d, 0x44, 0x6f, 0x76, 0x39, 0x20, 0xb, 0x12, 0x11, 0x8, 0x23,
        0x3a, 0x75, 0x6c, 0x47,
    ];
    for i in 0..input.len() {
        assert_eq!(expect[i], output1[i]);
    }
    mul_slice(25, &input, &mut output2);
    for i in 0..input.len() {
        assert_eq!(expect[i], output2[i]);
    }

    let expect_xor = [
        0x0, 0x2d, 0x5a, 0x77, 0xb4, 0x99, 0xee, 0x2f, 0x79, 0xf2, 0x7, 0x51, 0xd4, 0x19, 0x31,
        0xc9, 0xf8, 0xfc, 0xf9, 0x4f, 0x62, 0x15, 0x38, 0xfb, 0xd6, 0xa1, 0x8c, 0x96, 0xbb, 0xcc,
        0xe1, 0x22, 0xf, 0x78,
    ];
    mul_slice_xor(52, &input, &mut output1);
    for i in 0..input.len() {
        assert_eq!(expect_xor[i], output1[i]);
    }
    mul_slice_xor(52, &input, &mut output2);
    for i in 0..input.len() {
        assert_eq!(expect_xor[i], output2[i]);
    }

    let expect = [
        0x0, 0xb1, 0x7f, 0xce, 0xfe, 0x4f, 0x81, 0x9e, 0x3, 0x6, 0xe8, 0x75, 0xbd, 0x40, 0x36,
        0xa3, 0x95, 0xcb, 0xc, 0xdd, 0x6c, 0xa2, 0x13, 0x23, 0x92, 0x5c, 0xed, 0x1b, 0xaa, 0x64,
        0xd5, 0xe5, 0x54, 0x9a,
    ];
    mul_slice(177, &input, &mut output1);
    for i in 0..input.len() {
        assert_eq!(expect[i], output1[i]);
    }
    mul_slice(177, &input, &mut output2);
    for i in 0..input.len() {
        assert_eq!(expect[i], output2[i]);
    }

    let expect_xor = [
        0x0, 0xc4, 0x95, 0x51, 0x37, 0xf3, 0xa2, 0xfb, 0xec, 0xc5, 0xd0, 0xc7, 0x53, 0x88, 0xa3,
        0xa5, 0x6, 0x78, 0x97, 0x9f, 0x5b, 0xa, 0xce, 0xa8, 0x6c, 0x3d, 0xf9, 0xdf, 0x1b, 0x4a,
        0x8e, 0xe8, 0x2c, 0x7d,
    ];
    mul_slice_xor(117, &input, &mut output1);
    for i in 0..input.len() {
        assert_eq!(expect_xor[i], output1[i]);
    }
    mul_slice_xor(117, &input, &mut output2);
    for i in 0..input.len() {
        assert_eq!(expect_xor[i], output2[i]);
    }

    assert_eq!(exp(2, 2), 4);
    assert_eq!(exp(5, 20), 235);
    assert_eq!(exp(13, 7), 43);
}

#[test]
fn test_slice_add() {
    let length_list = [16, 32, 34];
    for len in length_list.iter() {
        let mut input = vec![0; *len];
        fill_random(&mut input);
        let mut output = vec![0; *len];
        fill_random(&mut output);
        let mut expect = vec![0; *len];
        for i in 0..expect.len() {
            expect[i] = input[i] ^ output[i];
        }
        scalar::slice_xor(&input, &mut output);
        for i in 0..expect.len() {
            assert_eq!(expect[i], output[i]);
        }
        fill_random(&mut output);
        for i in 0..expect.len() {
            expect[i] = input[i] ^ output[i];
        }
        scalar::slice_xor(&input, &mut output);
        for i in 0..expect.len() {
            assert_eq!(expect[i], output[i]);
        }
    }
}

#[test]
fn test_div_a_is_0() {
    assert_eq!(0, div(0, 100));
}

#[test]
fn test_div_b_is_0() {
    assert_eq!(0, div(1, 0));
}

#[test]
fn test_same_as_maybe_ffi() {
    let len = 10_003;
    for _ in 0..100 {
        let c = rand::random::<u8>();
        let mut input = vec![0; len];
        fill_random(&mut input);
        {
            let mut output = vec![0; len];
            fill_random(&mut output);
            let mut output_copy = output.clone();

            mul_slice(c, &input, &mut output);
            mul_slice(c, &input, &mut output_copy);

            assert_eq!(output, output_copy);
        }
        {
            let mut output = vec![0; len];
            fill_random(&mut output);
            let mut output_copy = output.clone();

            mul_slice_xor(c, &input, &mut output);
            mul_slice_xor(c, &input, &mut output_copy);

            assert_eq!(output, output_copy);
        }
    }
}

#[cfg(all(
    any(
        feature = "simd-neon",
        feature = "simd-ssse3",
        feature = "simd-avx2",
        feature = "simd-avx512",
        feature = "simd-gfni"
    ),
    any(target_arch = "x86_64", target_arch = "aarch64"),
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios")),
    feature = "std"
))]
#[test]
fn test_simd_c_matches_scalar_mul_slice() {
    let lengths = [0usize, 1, 15, 16, 17, 31, 32, 33, 255, 1024, 10_003];
    for &len in &lengths {
        for _ in 0..16 {
            let c = rand::random::<u8>();
            let mut input = vec![0; len];
            fill_random(&mut input);
            let mut scalar = vec![0; len];
            let mut simd = vec![0; len];

            mul_slice_scalar_for_test(c, &input, &mut scalar);
            legacy::simd_c::simd_c_mul_slice(c, &input, &mut simd);

            assert_eq!(scalar, simd);
        }
    }
}

#[cfg(all(
    any(
        feature = "simd-neon",
        feature = "simd-ssse3",
        feature = "simd-avx2",
        feature = "simd-avx512",
        feature = "simd-gfni"
    ),
    any(target_arch = "x86_64", target_arch = "aarch64"),
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios")),
    feature = "std"
))]
#[test]
fn test_simd_c_matches_scalar_mul_slice_xor() {
    let lengths = [0usize, 1, 15, 16, 17, 31, 32, 33, 255, 1024, 10_003];
    for &len in &lengths {
        for _ in 0..16 {
            let c = rand::random::<u8>();
            let mut input = vec![0; len];
            fill_random(&mut input);
            let mut scalar = vec![0; len];
            let mut simd = vec![0; len];
            fill_random(&mut scalar);
            simd.copy_from_slice(&scalar);

            mul_slice_xor_scalar_for_test(c, &input, &mut scalar);
            legacy::simd_c::simd_c_mul_slice_xor(c, &input, &mut simd);

            assert_eq!(scalar, simd);
        }
    }
}

#[cfg(all(
    feature = "simd-neon",
    target_arch = "aarch64",
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios")),
    feature = "std"
))]
#[test]
fn test_rust_neon_matches_scalar_mul_slice() {
    let _guard = NEON_PROFILE_TEST_LOCK
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let lengths = [0usize, 1, 15, 16, 17, 31, 32, 33, 255, 1024, 10_003];
    for &len in &lengths {
        for _ in 0..16 {
            let c = rand::random::<u8>();
            let mut input = vec![0; len];
            fill_random(&mut input);
            let mut scalar = vec![0; len];
            let mut neon = vec![0; len];

            mul_slice_scalar_for_test(c, &input, &mut scalar);
            aarch64::neon::rust_neon_mul_slice(c, &input, &mut neon);

            assert_eq!(scalar, neon);
        }
    }
}

#[cfg(all(
    feature = "simd-neon",
    target_arch = "aarch64",
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios")),
    feature = "std"
))]
#[test]
fn test_rust_neon_mul_slice_xor_c1_vectorized_fastpath() {
    let _guard = NEON_PROFILE_TEST_LOCK
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let lengths = [0usize, 1, 15, 16, 17, 31, 32, 33, 255, 1024, 10_003];
    for &len in &lengths {
        for _ in 0..16 {
            let mut input = vec![0; len];
            fill_random(&mut input);
            let mut scalar = vec![0; len];
            fill_random(&mut scalar);
            let mut neon = scalar.clone();

            mul_slice_xor_scalar_for_test(1, &input, &mut scalar);
            aarch64::neon::rust_neon_mul_slice_xor(1, &input, &mut neon);

            assert_eq!(scalar, neon);
        }
    }
}

#[cfg(all(
    feature = "simd-neon",
    target_arch = "aarch64",
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios")),
    feature = "std"
))]
#[test]
fn test_rust_neon_matches_scalar_mul_slice_xor() {
    let _guard = NEON_PROFILE_TEST_LOCK
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let lengths = [0usize, 1, 15, 16, 17, 31, 32, 33, 255, 1024, 10_003];
    for &len in &lengths {
        for _ in 0..16 {
            let c = rand::random::<u8>();
            let mut input = vec![0; len];
            fill_random(&mut input);
            let mut scalar = vec![0; len];
            let mut neon = vec![0; len];
            fill_random(&mut scalar);
            neon.copy_from_slice(&scalar);

            mul_slice_xor_scalar_for_test(c, &input, &mut scalar);
            aarch64::neon::rust_neon_mul_slice_xor(c, &input, &mut neon);

            assert_eq!(scalar, neon);
        }
    }
}

#[cfg(all(
    feature = "simd-neon",
    target_arch = "aarch64",
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios")),
    feature = "std"
))]
#[test]
fn test_rust_neon_matches_simd_c() {
    let _guard = NEON_PROFILE_TEST_LOCK
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let lengths = [0usize, 1, 15, 16, 17, 31, 32, 33, 255, 1024, 10_003];
    for &len in &lengths {
        for _ in 0..16 {
            let c = rand::random::<u8>();
            let mut input = vec![0; len];
            fill_random(&mut input);
            let mut simd_c = vec![0; len];
            let mut neon = vec![0; len];

            legacy::simd_c::simd_c_mul_slice(c, &input, &mut simd_c);
            aarch64::neon::rust_neon_mul_slice(c, &input, &mut neon);

            assert_eq!(simd_c, neon);
        }
    }
}

#[cfg(all(
    feature = "simd-neon",
    target_arch = "aarch64",
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios")),
    feature = "std"
))]
#[test]
fn test_rust_neon_profile_stats_track_vector_vs_tail() {
    let _guard = NEON_PROFILE_TEST_LOCK
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    reset_rust_neon_profile_stats();

    let c = 25u8;
    let mut input = vec![0u8; 65];
    fill_random(&mut input);
    let mut out = vec![0u8; 65];
    let mut out_xor = vec![0u8; 65];

    let before = rust_neon_profile_stats();
    aarch64::neon::rust_neon_mul_slice(c, &input, &mut out);
    aarch64::neon::rust_neon_mul_slice_xor(c, &input, &mut out_xor);
    let delta = rust_neon_profile_stats().saturating_sub(before);

    assert_eq!(1, delta.mul_calls);
    assert_eq!(1, delta.mul_xor_calls);
    assert_eq!(130, delta.total_bytes);
    assert_eq!(2, delta.vector_64b_chunks);
    assert_eq!(0, delta.vector_16b_chunks);
    assert_eq!(2, delta.tail_bytes);
    assert_eq!(2, delta.tail_calls);
    assert_eq!(16, delta.table_lookups);
}

#[cfg(feature = "std")]
#[test]
fn test_parse_rust_neon_xor_unroll() {
    assert_eq!(Some(2), parse_rust_neon_xor_unroll("2"));
    assert_eq!(Some(4), parse_rust_neon_xor_unroll("4"));
    assert_eq!(None, parse_rust_neon_xor_unroll("1"));
    assert_eq!(None, parse_rust_neon_xor_unroll("8"));
    assert_eq!(None, parse_rust_neon_xor_unroll("abc"));
}

#[cfg(feature = "std")]
#[test]
fn test_rust_neon_xor_schedule_env_constant() {
    assert_eq!(
        "RS_NEON_MUL_SLICE_XOR_SCHEDULE",
        RS_NEON_MUL_SLICE_XOR_SCHEDULE_ENV
    );
}

#[test]
fn test_active_backend_metadata() {
    #[cfg(all(
        any(
            feature = "simd-neon",
            feature = "simd-ssse3",
            feature = "simd-avx2",
            feature = "simd-avx512",
            feature = "simd-gfni"
        ),
        any(target_arch = "x86_64", target_arch = "aarch64"),
        not(target_env = "msvc"),
        not(any(target_os = "android", target_os = "ios"))
    ))]
    {
        #[cfg(all(feature = "std", target_arch = "x86_64"))]
        {
            let has_gfni = std::is_x86_feature_detected!("gfni");
            let has_avx512 = std::is_x86_feature_detected!("avx512f")
                && std::is_x86_feature_detected!("avx512bw");
            let has_avx2 = std::is_x86_feature_detected!("avx2");

            // GFNI backends are auto-selected with higher priority when available.
            if has_gfni && has_avx512 {
                assert_eq!(active_backend_name(), "rust-gfni-avx512");
                assert_eq!(active_backend_kind(), BackendKind::RustSimd);
            } else if has_gfni && has_avx2 {
                assert_eq!(active_backend_name(), "rust-gfni-avx2");
                assert_eq!(active_backend_kind(), BackendKind::RustSimd);
            } else if has_avx2 {
                assert_eq!(active_backend_name(), "rust-avx2");
                assert_eq!(active_backend_kind(), BackendKind::RustSimd);
            } else if has_avx512 {
                assert_eq!(active_backend_name(), "rust-avx512");
                assert_eq!(active_backend_kind(), BackendKind::RustSimd);
            } else if std::is_x86_feature_detected!("ssse3") {
                assert_eq!(active_backend_name(), "rust-ssse3");
                assert_eq!(active_backend_kind(), BackendKind::RustSimd);
            } else if cfg!(rse_simd_c_build_baseline) && std::is_x86_feature_detected!("sse2") {
                assert_eq!(active_backend_name(), "simd-c");
                assert_eq!(active_backend_kind(), BackendKind::SimdC);
            } else {
                assert_eq!(active_backend_name(), "scalar-rust");
                assert_eq!(active_backend_kind(), BackendKind::Scalar);
            }

            if has_gfni && has_avx512 {
                with_env_var("RSE_BACKEND_OVERRIDE", "rust-gfni-avx512", || {
                    assert_eq!(
                        super::backend::runtime_override_backend_name_for_test(),
                        Some("rust-gfni-avx512")
                    );
                });
            }
        }

        #[cfg(all(feature = "std", target_arch = "aarch64"))]
        {
            assert_eq!(active_backend_name(), "rust-neon");
            assert_eq!(active_backend_kind(), BackendKind::RustSimd);
        }

        #[cfg(not(feature = "std"))]
        {
            assert_eq!(active_backend_name(), "scalar-rust");
            assert_eq!(active_backend_kind(), BackendKind::Scalar);
        }
    }

    #[cfg(not(all(
        any(
            feature = "simd-neon",
            feature = "simd-ssse3",
            feature = "simd-avx2",
            feature = "simd-avx512",
            feature = "simd-gfni"
        ),
        any(target_arch = "x86_64", target_arch = "aarch64"),
        not(target_env = "msvc"),
        not(any(target_os = "android", target_os = "ios"))
    )))]
    {
        assert_eq!(active_backend_name(), "scalar-rust");
        assert_eq!(active_backend_kind(), BackendKind::Scalar);
    }
}

#[cfg(all(
    any(
        feature = "simd-neon",
        feature = "simd-ssse3",
        feature = "simd-avx2",
        feature = "simd-avx512",
        feature = "simd-gfni"
    ),
    feature = "std",
    any(target_arch = "x86_64", target_arch = "aarch64"),
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios"))
))]
#[test]
fn test_backend_override_affects_active_backend() {
    #[cfg(target_arch = "aarch64")]
    {
        with_env_var("RSE_BACKEND_OVERRIDE", "scalar", || {
            assert_eq!(
                super::backend::runtime_override_backend_name_for_test(),
                Some("scalar-rust")
            );
            assert_eq!(
                super::backend::runtime_override_backend_id_for_test(),
                Some(BackendId::ScalarRust)
            );
        });

        with_env_var("RSE_BACKEND_OVERRIDE", "rust-neon", || {
            assert_eq!(
                super::backend::runtime_override_backend_name_for_test(),
                Some("rust-neon")
            );
            assert_eq!(
                super::backend::runtime_override_backend_id_for_test(),
                Some(BackendId::RustNeon)
            );
        });
    }

    #[cfg(target_arch = "x86_64")]
    {
        with_env_var("RSE_BACKEND_OVERRIDE", "rust-gfni-avx2", || {
            let gfni_name = super::backend::runtime_override_backend_name_for_test();
            let gfni_id = super::backend::runtime_override_backend_id_for_test();
            if std::is_x86_feature_detected!("gfni") && std::is_x86_feature_detected!("avx2") {
                assert_eq!(gfni_name, Some("rust-gfni-avx2"));
                assert_eq!(gfni_id, Some(BackendId::RustGfniAvx2));
            } else {
                assert_eq!(gfni_name, None);
                assert_eq!(gfni_id, None);
            }
        });

        with_env_var("RSE_BACKEND_OVERRIDE", "rust-gfni-avx512", || {
            let gfni512_name = super::backend::runtime_override_backend_name_for_test();
            let gfni512_id = super::backend::runtime_override_backend_id_for_test();
            if std::is_x86_feature_detected!("gfni")
                && std::is_x86_feature_detected!("avx512f")
                && std::is_x86_feature_detected!("avx512bw")
            {
                assert_eq!(gfni512_name, Some("rust-gfni-avx512"));
                assert_eq!(gfni512_id, Some(BackendId::RustGfniAvx512));
            } else {
                assert_eq!(gfni512_name, None);
                assert_eq!(gfni512_id, None);
            }
        });

        with_env_var("RSE_BACKEND_OVERRIDE", "rust-avx512", || {
            let avx512_name = super::backend::runtime_override_backend_name_for_test();
            let avx512_id = super::backend::runtime_override_backend_id_for_test();
            if std::is_x86_feature_detected!("avx512f") && std::is_x86_feature_detected!("avx512bw")
            {
                assert_eq!(avx512_name, Some("rust-avx512"));
                assert_eq!(avx512_id, Some(BackendId::RustAvx512));
            } else {
                assert_eq!(avx512_name, None);
                assert_eq!(avx512_id, None);
            }
        });

        with_env_var("RSE_BACKEND_OVERRIDE", "rust-ssse3", || {
            let ssse3_name = super::backend::runtime_override_backend_name_for_test();
            let ssse3_id = super::backend::runtime_override_backend_id_for_test();
            if std::is_x86_feature_detected!("ssse3") {
                assert_eq!(ssse3_name, Some("rust-ssse3"));
                assert_eq!(ssse3_id, Some(BackendId::RustSsse3));
            } else {
                assert_eq!(ssse3_name, None);
                assert_eq!(ssse3_id, None);
            }
        });

        with_env_var("RSE_BACKEND_OVERRIDE", "rust-avx2", || {
            let avx2_name = super::backend::runtime_override_backend_name_for_test();
            let avx2_id = super::backend::runtime_override_backend_id_for_test();
            if std::is_x86_feature_detected!("avx2") {
                assert_eq!(avx2_name, Some("rust-avx2"));
                assert_eq!(avx2_id, Some(BackendId::RustAvx2));
            } else {
                assert_eq!(avx2_name, None);
                assert_eq!(avx2_id, None);
            }
        });
    }
}

#[cfg(all(
    feature = "simd-neon",
    feature = "std",
    target_arch = "aarch64",
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios"))
))]
#[test]
fn test_active_backend_metadata_fresh_process() {
    use std::process::Command;

    if std::env::var("RSE_GALOIS8_CHILD_CHECK").as_deref() == Ok("active-backend-metadata") {
        println!("child_backend={}", active_backend_name());
        println!("child_backend_kind={:?}", active_backend_kind());
        return;
    }

    let current_exe = std::env::current_exe().unwrap();
    let output = Command::new(current_exe)
        .env_remove("RSE_BACKEND_OVERRIDE")
        .env_remove("RSE_STRICT_BACKEND_OVERRIDE")
        .env("RSE_GALOIS8_CHILD_CHECK", "active-backend-metadata")
        .arg("--exact")
        .arg("galois_8::tests::test_active_backend_metadata_fresh_process")
        .arg("--nocapture")
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "child active backend metadata check failed: stdout={} stderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("child_backend=rust-neon"), "{stdout}");
    assert!(stdout.contains("child_backend_kind=RustSimd"), "{stdout}");
}

#[cfg(all(
    feature = "simd-neon",
    feature = "std",
    target_arch = "aarch64",
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios"))
))]
#[test]
fn test_aarch64_backend_override_metadata_matches_expected_ids() {
    with_env_var("RSE_BACKEND_OVERRIDE", "scalar", || {
        assert_eq!(
            super::backend::runtime_override_backend_name_for_test(),
            Some("scalar-rust")
        );
        assert_eq!(
            super::backend::runtime_override_backend_id_for_test(),
            Some(BackendId::ScalarRust)
        );
    });

    with_env_var("RSE_BACKEND_OVERRIDE", "rust-neon", || {
        assert_eq!(
            super::backend::runtime_override_backend_name_for_test(),
            Some("rust-neon")
        );
        assert_eq!(
            super::backend::runtime_override_backend_id_for_test(),
            Some(BackendId::RustNeon)
        );
    });
}

#[cfg(all(
    feature = "simd-neon",
    target_arch = "x86_64",
    not(target_env = "msvc"),
    not(any(target_os = "android", target_os = "ios")),
    feature = "std"
))]
#[test]
fn test_x86_cross_backend_conformance_matrix() {
    let has_sse2 = std::is_x86_feature_detected!("sse2");
    let has_ssse3 = std::is_x86_feature_detected!("ssse3");
    let has_avx2 = std::is_x86_feature_detected!("avx2");
    let has_avx512 =
        std::is_x86_feature_detected!("avx512f") && std::is_x86_feature_detected!("avx512bw");
    let has_gfni = std::is_x86_feature_detected!("gfni") && has_avx2;

    let lengths = [
        0usize, 1, 15, 16, 17, 31, 32, 33, 63, 64, 65, 255, 1024, 10_003,
    ];
    let coeffs = [0u8, 1, 2, 15, 16, 31, 127, 173, 255];

    for &len in &lengths {
        for &c in &coeffs {
            let mut input = vec![0; len];
            fill_random(&mut input);

            let mut scalar = vec![0; len];
            mul_slice_scalar_for_test(c, &input, &mut scalar);

            let mut scalar_xor = vec![0; len];
            fill_random(&mut scalar_xor);
            let xor_seed = scalar_xor.clone();
            mul_slice_xor_scalar_for_test(c, &input, &mut scalar_xor);

            if cfg!(rse_simd_c_build_baseline) && has_sse2 {
                let mut simd_c = vec![0; len];
                legacy::simd_c::simd_c_mul_slice(c, &input, &mut simd_c);
                assert_eq!(scalar, simd_c, "simd-c mismatch len={len} coeff={c}");

                let mut simd_c_xor = xor_seed.clone();
                legacy::simd_c::simd_c_mul_slice_xor(c, &input, &mut simd_c_xor);
                assert_eq!(
                    scalar_xor, simd_c_xor,
                    "simd-c xor mismatch len={len} coeff={c}"
                );
            }

            if has_ssse3 {
                let mut ssse3 = vec![0; len];
                x86::ssse3::rust_ssse3_mul_slice(c, &input, &mut ssse3);
                assert_eq!(scalar, ssse3, "ssse3 mismatch len={len} coeff={c}");

                let mut ssse3_xor = xor_seed.clone();
                x86::ssse3::rust_ssse3_mul_slice_xor(c, &input, &mut ssse3_xor);
                assert_eq!(
                    scalar_xor, ssse3_xor,
                    "ssse3 xor mismatch len={len} coeff={c}"
                );
            }

            if has_avx2 {
                let mut avx2 = vec![0; len];
                x86::avx2::rust_avx2_mul_slice(c, &input, &mut avx2);
                assert_eq!(scalar, avx2, "avx2 mismatch len={len} coeff={c}");

                let mut avx2_xor = xor_seed.clone();
                x86::avx2::rust_avx2_mul_slice_xor(c, &input, &mut avx2_xor);
                assert_eq!(
                    scalar_xor, avx2_xor,
                    "avx2 xor mismatch len={len} coeff={c}"
                );
            }

            if has_avx512 {
                let mut avx512 = vec![0; len];
                x86::avx512::rust_avx512_mul_slice(c, &input, &mut avx512);
                assert_eq!(scalar, avx512, "avx512 mismatch len={len} coeff={c}");

                let mut avx512_xor = xor_seed.clone();
                x86::avx512::rust_avx512_mul_slice_xor(c, &input, &mut avx512_xor);
                assert_eq!(
                    scalar_xor, avx512_xor,
                    "avx512 xor mismatch len={len} coeff={c}"
                );
            }

            if has_gfni {
                let mut gfni = vec![0; len];
                x86::gfni::rust_gfni_avx2_mul_slice(c, &input, &mut gfni);
                assert_eq!(scalar, gfni, "gfni mismatch len={len} coeff={c}");

                let mut gfni_xor = xor_seed.clone();
                x86::gfni::rust_gfni_avx2_mul_slice_xor(c, &input, &mut gfni_xor);
                assert_eq!(
                    scalar_xor, gfni_xor,
                    "gfni xor mismatch len={len} coeff={c}"
                );
            }
        }
    }
}
