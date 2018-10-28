#![allow(dead_code)]
use libc;

include!(concat!(env!("OUT_DIR"), "/table.rs"));

pub fn add(a : u8, b : u8) -> u8 {
    a ^ b
}

pub fn sub(a : u8, b : u8) -> u8 {
    a ^ b
}

pub fn mul(a : u8, b : u8) -> u8 {
    MUL_TABLE[a as usize][b as usize]
}

pub fn div(a : u8, b : u8) -> u8 {
    if        a == 0 {
        0
    } else if b == 0 {
        panic!("Divisor is 0")
    } else {
        let log_a = LOG_TABLE[a as usize];
        let log_b = LOG_TABLE[b as usize];
        let mut log_result = log_a as isize - log_b as isize;
        if log_result < 0 {
            log_result += 255;
        }
        EXP_TABLE[log_result as usize]
    }
}

pub fn exp(a : u8, n : usize) -> u8 {
    if        n == 0 {
        1
    } else if a == 0 {
        0
    } else {
        let log_a = LOG_TABLE[a as usize];
        let mut log_result = log_a as usize * n;
        while 255 <= log_result {
            log_result -= 255;
        }
        EXP_TABLE[log_result]
    }
}

const PURE_RUST_UNROLL : isize = 4;

macro_rules! return_if_empty {
    (
        $len:expr
    ) => {
        if $len == 0 { return; }
    }
}


#[cfg(
    not(
        all(
            not(feature = "pure-rust"),
            any(target_arch = "x86_64", target_arch = "aarch64"),
            not(any(target_os="android", target_os="androideabi", target_os="ios"))
        )
    )
)]
pub fn mul_slice(c : u8, input : &[u8], out : &mut [u8]) {
    mul_slice_pure_rust(c, input, out);
}

#[cfg(
    not(
        all(
            not(feature = "pure-rust"),
            any(target_arch = "x86_64", target_arch = "aarch64"),
            not(any(target_os="android", target_os="androideabi", target_os="ios"))
        )
    )
)]
pub fn mul_slice_xor(c : u8, input : &[u8], out : &mut [u8]) {
    mul_slice_xor_pure_rust(c, input, out);
}

pub fn mul_slice_pure_rust(c : u8, input : &[u8], out : &mut [u8]) {
    let mt                 = &MUL_TABLE[c as usize];
    let mt_ptr : *const u8 = &mt[0];

    assert_eq!(input.len(), out.len());

    let len : isize = input.len() as isize;
    return_if_empty!(len);

    let mut input_ptr : *const u8 = &input[0];
    let mut out_ptr   : *mut   u8 = &mut out[0];

    let mut n : isize = 0;
    unsafe {
        assert_eq!(4, PURE_RUST_UNROLL);
        if len > PURE_RUST_UNROLL {
            let len_minus_unroll = len - PURE_RUST_UNROLL;
            while n < len_minus_unroll {
                *out_ptr           = *mt_ptr.offset(*input_ptr           as isize);
                *out_ptr.offset(1) = *mt_ptr.offset(*input_ptr.offset(1) as isize);
                *out_ptr.offset(2) = *mt_ptr.offset(*input_ptr.offset(2) as isize);
                *out_ptr.offset(3) = *mt_ptr.offset(*input_ptr.offset(3) as isize);

                input_ptr = input_ptr.offset(PURE_RUST_UNROLL);
                out_ptr   =   out_ptr.offset(PURE_RUST_UNROLL);
                n        += PURE_RUST_UNROLL;
            }
        }
        while n < len {
            *out_ptr  = *mt_ptr.offset(*input_ptr as isize);

            input_ptr = input_ptr.offset(1);
            out_ptr   =   out_ptr.offset(1);
            n += 1;
        }
    }
    /* for n in 0..input.len() {
     *   out[n] = mt[input[n] as usize]
     * }
     */
}

pub fn mul_slice_xor_pure_rust(c : u8, input : &[u8], out : &mut [u8]) {
    let mt                 = &MUL_TABLE[c as usize];
    let mt_ptr : *const u8 = &mt[0];

    assert_eq!(input.len(), out.len());

    let len : isize = input.len() as isize;
    return_if_empty!(len);

    let mut input_ptr : *const u8 = &input[0];
    let mut out_ptr   : *mut   u8 = &mut out[0];

    let mut n : isize = 0;
    unsafe {
        assert_eq!(4, PURE_RUST_UNROLL);
        if len > PURE_RUST_UNROLL {
            let len_minus_unroll = len - PURE_RUST_UNROLL;
            while n < len_minus_unroll {
                *out_ptr           ^= *mt_ptr.offset(*input_ptr           as isize);
                *out_ptr.offset(1) ^= *mt_ptr.offset(*input_ptr.offset(1) as isize);
                *out_ptr.offset(2) ^= *mt_ptr.offset(*input_ptr.offset(2) as isize);
                *out_ptr.offset(3) ^= *mt_ptr.offset(*input_ptr.offset(3) as isize);

                input_ptr = input_ptr.offset(PURE_RUST_UNROLL);
                out_ptr   =   out_ptr.offset(PURE_RUST_UNROLL);
                n        += PURE_RUST_UNROLL;
            }
        }
        while n < len {
            *out_ptr ^= *mt_ptr.offset(*input_ptr as isize);

            input_ptr = input_ptr.offset(1);
            out_ptr   =   out_ptr.offset(1);
            n        += 1;
        }
    }
    /* for n in 0..input.len() {
     *   out[n] ^= mt[input[n] as usize];
     * }
     */
}

pub fn slice_xor(input : &[u8], out : &mut [u8]) {
    assert_eq!(input.len(), out.len());

    let len   : isize = input.len() as isize;
    return_if_empty!(len);

    let mut input_ptr : *const u8 = &input[0];
    let mut out_ptr   : *mut   u8 = &mut out[0];

    let mut n : isize = 0;
    unsafe {
        assert_eq!(4, PURE_RUST_UNROLL);
        if len > PURE_RUST_UNROLL {
            let len_minus_unroll = len - PURE_RUST_UNROLL;
            while n < len_minus_unroll {
                *out_ptr           ^= *input_ptr;
                *out_ptr.offset(1) ^= *input_ptr.offset(1);
                *out_ptr.offset(2) ^= *input_ptr.offset(2);
                *out_ptr.offset(3) ^= *input_ptr.offset(3);

                input_ptr = input_ptr.offset(PURE_RUST_UNROLL);
                out_ptr   =   out_ptr.offset(PURE_RUST_UNROLL);
                n        += PURE_RUST_UNROLL;
            }
        }
        while n < len {
            *out_ptr ^= *input_ptr;

            input_ptr = input_ptr.offset(1);
            out_ptr   =   out_ptr.offset(1);
            n        += 1;
        }
    }
    /* for n in 0..input.len() {
     *   out[n] ^= input[n]
     * }
     */
}

#[cfg(
    all(
        not(feature = "pure-rust"),
        any(target_arch = "x86_64", target_arch = "aarch64"),
        not(any(target_os="android", target_os="androideabi", target_os="ios"))
    )
)]
extern {
    fn reedsolomon_gal_mul(low   : *const libc::uint8_t,
                           high  : *const libc::uint8_t,
                           input : *const libc::uint8_t,
                           out   : *mut   libc::uint8_t,
                           len   : libc::size_t)
                           -> libc::size_t;

    fn reedsolomon_gal_mul_xor(low   : *const libc::uint8_t,
                               high  : *const libc::uint8_t,
                               input : *const libc::uint8_t,
                               out   : *mut   libc::uint8_t,
                               len   : libc::size_t)
                               -> libc::size_t;
}

#[cfg(
    all(
        not(feature = "pure-rust"),
        any(target_arch = "x86_64", target_arch = "aarch64"),
        not(any(target_os="android", target_os="androideabi", target_os="ios"))
    )
)]
pub fn mul_slice(c : u8, input : &[u8], out : &mut [u8]) {
    let low  : *const libc::uint8_t = &MUL_TABLE_LOW[c as usize][0];
    let high : *const libc::uint8_t = &MUL_TABLE_HIGH[c as usize][0];

    assert_eq!(input.len(), out.len());

    let input_ptr : *const libc::uint8_t = &input[0];
    let out_ptr   : *mut   libc::uint8_t = &mut out[0];
    let size      : libc::size_t         = input.len();

    let bytes_done : usize = unsafe {
        reedsolomon_gal_mul(low, high, input_ptr, out_ptr, size) as usize
    };

    mul_slice_pure_rust(c,
                        &input[bytes_done..],
                        &mut out[bytes_done..]);
}

#[cfg(
    all(
        not(feature = "pure-rust"),
        any(target_arch = "x86_64", target_arch = "aarch64"),
        not(any(target_os="android", target_os="androideabi", target_os="ios"))
    )
)]
pub fn mul_slice_xor(c : u8, input : &[u8], out : &mut [u8]) {
    let low  : *const libc::uint8_t = &MUL_TABLE_LOW[c as usize][0];
    let high : *const libc::uint8_t = &MUL_TABLE_HIGH[c as usize][0];

    assert_eq!(input.len(), out.len());

    let input_ptr : *const libc::uint8_t = &input[0];
    let out_ptr   : *mut   libc::uint8_t = &mut out[0];
    let size      : libc::size_t         = input.len();

    let bytes_done : usize = unsafe {
        reedsolomon_gal_mul_xor(low, high, input_ptr, out_ptr, size) as usize
    };

    mul_slice_xor_pure_rust(c,
                            &input[bytes_done..],
                            &mut out[bytes_done..]);
}
