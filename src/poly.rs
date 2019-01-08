//! Polynomial over GF(256)
//!
//! adapted from https://github.com/mersinvald/reed-solomon-rs

use std::cmp;
use std::ops::{Add, Div, Mul, MulAssign, Deref};
use std::fmt;
use crate::galois_8;

const POLYNOMIAL_MAX_LENGTH: usize = 256;

// create polynomial with given big-endian coefficients
macro_rules! polynom {
    [$value:expr; $count:expr] => {{
        let array = [$value; $count];
        $crate::poly::Polynom::from(&array[..])
    }}; 

    [$( $value:expr ),* ] => {{
        let array = [$($value, )*];
        $crate::poly::Polynom::from(&array[..])
    }};
}

/// A polynomial with coefficients in GF(2^8).
/// The most significant coefficient is at the front and is never zero.
#[derive(Clone)]
pub struct Polynom {
    array: [u8; POLYNOMIAL_MAX_LENGTH],
    length: usize,
}

impl Polynom {
    /// Creates a new zero polynomial; e.g. 
    pub fn new() -> Polynom {
        Polynom {
            array: [0; POLYNOMIAL_MAX_LENGTH],
            length: 0,
        }
    }

    /// The number of coefficients necessary to represent this polynomial.
    /// This is the degree + 1.
    pub fn len(&self) -> usize {
        self.length
    }

    /// Push a coefficient onto the polynomial. Note that a polynomial
    /// with all zero coefficients will 
    pub fn push(&mut self, x: u8) {
        if self.length == 1 && self.array[0] == 0 {
            self.array[0] = x;
        } else {
            self.array[self.length] = x;
            self.length += 1;
        }
    }

    /// Set a coefficient. Panics if it is out of bounds.
    /// If the leading coefficient (index 0) is set to 0, this will
    /// reduce the degree of the polynomial.
    pub fn set_coefficient(&mut self, pos: usize, c: u8) {
        self.array[..self.length][pos] = c;

        if pos == 0 && c == 0 {
            // reduce the degree once a leading coefficient has been set down,
            self.minimize()
        }
    }

    /// Evaluate the polynomial.
    pub fn eval(&self, x: u8) -> u8 {
        if self.is_zero() { return 0 }

        let mut y = self[0];
        for px in self.iter().skip(1) {
            y = galois_8::mul(y, x) ^ px;
        }
        y
    }

    /// If the polynomial is zero.
    #[inline]
    pub fn is_zero(&self) -> bool {
        self.is_empty() || self.length == 1 && self[0] == 0
    }

    /// Run the extended eucidean algorithm with self and `rhs`.
    pub fn egcd(&self, rhs: &Polynom) -> (Polynom, Polynom, Polynom) {
        if self.is_zero() {
            (rhs.clone(), polynom![0], polynom![1])
        } else {
            let (cur_quotient, cur_remainder) = rhs.div(self);
            let (g, x, y) = cur_remainder.egcd(self);
            (g, &y + &(&cur_quotient * &x), x)
        }
    }

    fn minimize(&mut self) {
        let leading_zeros = self.iter().cloned().take_while(|&x| x == 0).count();
        self.array.rotate_left(leading_zeros);
        self.length = self.length - leading_zeros;
    }

    fn set_length(&mut self, new_len: usize) {
        let old_len = self.len();
        self.length = new_len;
        
        if new_len > old_len {
            for x in &mut self.array[old_len..new_len] {
                *x = 0;
            }
        } else if new_len < old_len {
            for x in &mut self.array[new_len..old_len] {
                *x = 0;
            }
        }
    }
}

impl Default for Polynom {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for Polynom {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        let len = self.len();
        &self.array[0..len]
    }
}

impl<'a> From<&'a [u8]> for Polynom {
    fn from(slice: &'a [u8]) -> Polynom {
        // ignore leading zeros.
        let leading_zeros = slice.iter().cloned().take_while(|&x| x == 0).count();
        let slice = &slice[leading_zeros..];

        debug_assert!(slice.len() <= POLYNOMIAL_MAX_LENGTH);

        let mut poly = Polynom::new();
        poly.length = slice.len();
        poly.array[..slice.len()].copy_from_slice(slice);
        poly
    }
}

impl fmt::Debug for Polynom {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}", &self[..])
    }
}

impl PartialEq for Polynom {
    fn eq(&self, other: &Polynom) -> bool {
        if self.length == other.length {
            &self[..] == &other[..]
        } else {
            let self_first_sig_index = self.iter().take_while(|&&i| i == 0).count();
            let other_first_sig_index = other.iter().take_while(|&&i| i == 0).count();

            &self[self_first_sig_index..] == &other[other_first_sig_index..]
        }
    }
}

impl<'a> Add<&'a Polynom> for &'a Polynom {
    type Output = Polynom;

    fn add(self, rhs: Self) -> Polynom {
        let mut poly = Polynom::new();
        poly.length = cmp::max(self.len(), rhs.len());

        for (i, x) in self.iter().enumerate() {
            let index = i + poly.len() - self.len();
            poly.array[index] = *x;
        }

        for (i, x) in rhs.iter().enumerate() {
            let index = i + poly.len() - rhs.len();
            poly.array[index] ^= *x;
        }

        poly.minimize();

        poly
    }
}

// scalar multiplication
impl<'a> Mul<u8> for &'a Polynom {
    type Output = Polynom;

    #[inline]
    fn mul(self, x: u8) -> Polynom {
        let mut poly = self.clone();
        poly.mul_assign(x);
        poly
    }
}

impl MulAssign<u8> for Polynom {
    fn mul_assign(&mut self, x: u8) {
        for px in self.array[..self.length].iter_mut() {
            *px = galois_8::mul(*px, x);
        }

        self.minimize();
    }
}

impl<'a> Mul<&'a Polynom> for &'a Polynom {
    type Output = Polynom;

    #[inline]
    fn mul(self, rhs: Self) -> Polynom {
        let mut poly = Polynom::new();

        poly.length = (self.len() + rhs.len()).saturating_sub(1);

        for (j, rhs_x) in rhs.iter().enumerate() {
            for (i, self_x) in self.iter().enumerate() {
                poly.array[i + j] ^= galois_8::mul(*self_x, *rhs_x);
            }
        }

        poly.minimize();
        poly    
    }
}

impl<'a> Div<&'a Polynom> for &'a Polynom {
    type Output = (Polynom, Polynom);

    fn div(self, rhs: Self) -> (Polynom, Polynom) {
        if rhs.is_zero() {
            panic!("Divisor is 0")
        }

        let mut poly = self.clone();

        // If divisor's degree (len-1) is bigger, all dividend is a remainder
        let divisor_degree = rhs.len() - 1;
        if self.len() <= divisor_degree {
            return (Polynom::new(), poly);
        }

        let leading_mul_inv = galois_8::div(1,rhs[0]);
        let monictized = rhs * leading_mul_inv;

        for i in 0..(self.len() - divisor_degree) {
            let coef = poly[i];
            if coef != 0 {
                for j in 1..monictized.len() {
                    if rhs[j] != 0 {
                        poly.array[i + j] ^= galois_8::mul(monictized[j], coef); // c*x^(i+j)  = a*x^i*b*x^j
                    }
                }
            }
        }

        let separator = self.len().saturating_sub(divisor_degree);

        // Remainder is after separator
        let remainder = Polynom::from(&poly[separator..]);

        // And quotient is before separator, so just shrink to it
        poly.set_length(separator);
        poly *= leading_mul_inv;

        if cfg!(debug_assertions) {
            // check result in debug mode.
            let res = &(&poly * rhs) + &remainder;
            assert_eq!(&res, self);
        }

        (poly, remainder)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::{Arbitrary, Gen};

    impl Arbitrary for Polynom {
        fn arbitrary<G: Gen>(gen: &mut G) -> Self {
            const MAX_TEST_LEN: usize = 32;

            let len = usize::arbitrary(gen) % MAX_TEST_LEN;
            if len == 0 { return Polynom::new() }

            let leading = loop {
                let x = u8::arbitrary(gen);
                if x != 0 { break x }
            };

            let mut poly = Polynom::new();
            poly.push(leading);

            for _ in 1..len {
                poly.push(u8::arbitrary(gen));
            }

            poly
        }
    }

    quickcheck! {
        fn qc_add_associativity(a: Polynom, b: Polynom, c: Polynom) -> bool {
            &a + &(&b + &c) == &(&a + &b) + &c
        }

        fn qc_mul_associativity(a: Polynom, b: Polynom, c: Polynom) -> bool {
            &a * &(&b * &c) == &(&a * &b) * &c
        }

        fn qc_multiplicative_identity(a: Polynom) -> bool {
            a.is_zero() || {
                let one = polynom![1];
                let (q, r) = &one / &a;
                &(&q * &a) + &r == one
            }
        }

        fn qc_add_commutativity(a: Polynom, b: Polynom) -> bool {
            &a + &b == &b + &a
        }

        fn qc_mul_commutativity(a: Polynom, b: Polynom) -> bool {
            &a * &b == &b * &a
        }

        fn qc_add_distributivity(a: Polynom, b: Polynom, c: Polynom) -> bool {
            &a * &(&b + &c) == &(&a * &b) + &(&a * &c)
        }

        fn qc_egcd(a: Polynom, b: Polynom) -> bool {
            let (gcd, x, y) = a.egcd(&b);

            // bezout's identity
            &(&a * &x) + &(&b * &y) == gcd
        }
    }

    #[test]
    #[should_panic]
    fn divide_by_zero() {
        let p = polynom![1, 2, 3];
        let zero = polynom![];

        let _ = &p / &zero;
    }

    #[test]
    fn set_length() {
        let mut poly = polynom![1; 8];
        poly.set_length(2);
        poly.set_length(6);

        for i in 0..2 {
            assert_eq!(poly.array[i], 1);
        }

        for i in 2..6 {
            assert_eq!(poly.array[i], 0);
        }
    }

    #[test]
    fn scale() {
        let poly = polynom![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let answer = polynom![0, 3, 6, 5, 12, 15, 10, 9, 24, 27];
        assert_eq!(answer, &poly * 3);
    }

    #[test]
    fn scale_assign() {
        let mut poly = polynom![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let answer = polynom![0, 3, 6, 5, 12, 15, 10, 9, 24, 27];

        poly *= 3;
        assert_eq!(answer, poly);
    }

    #[test]
    fn add() {
        let px = polynom![0, 5, 10, 15, 20];
        let py = polynom![3, 9, 17, 24, 75];
        assert_eq!([3, 12, 27, 23, 95], *(&px + &py));

        let px = polynom![0, 5, 10];
        let py = polynom![3, 9, 17, 24, 75];

        assert_eq!([3, 9, 17, 29, 65], *(&px + &py));
        assert_eq!([3, 9, 17, 29, 65], *(&py + &px))
    }

    #[test]
    fn mul() {
        let px = polynom![0, 5, 10, 15, 20];
        let py = polynom![3, 9, 17, 24, 75];
        assert_eq!([15, 51, 30, 153, 193, 53, 115, 245], *(&px * &py));

        let px = polynom![0, 5, 10];
        let py = polynom![3, 9, 17, 24, 75];

        assert_eq!([15, 51, 15, 210, 138, 244], *(&px * &py));
        assert_eq!([15, 51, 15, 210, 138, 244], *(&py * &px));
    }


    #[test]
    fn div() {
        let px = polynom![0, 5, 10, 15, 20];
        let py = polynom![3, 9, 17, 24, 75];

        let empty: [u8; 0] = [];

        let (q, r) = &px / &py;
        assert_eq!(empty, *q);
        assert_eq!([5, 10, 15, 20], *r);

        let (q, r) = &py / &px;
        assert_eq!([244, 3], *q);
        assert_eq!([10, 5, 119], *r);

        let px = polynom![0, 5, 10];
        let py = polynom![3, 9, 17, 24, 75];

        let (q, r) = &px / &py;

        assert_eq!(empty, *q);
        assert_eq!([5, 10], *r);

        let (q, r) = &py / &px;
        assert_eq!([244, 3, 3, 245], *q);
        assert_eq!([71], *r);
    }

    #[test]
    fn eval() {
        let p = polynom![0, 5, 10, 15, 20];
        let tests = [4, 7, 21, 87, 35, 255];
        let answers = [213, 97, 132, 183, 244, 92];

        for i in 0..tests.len() {
            assert_eq!(answers[i], p.eval(tests[i]));
        }
    }
}
