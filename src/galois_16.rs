//! GF(2^16) implementation.
//!
//! More accurately, this is a `GF((2^8)^2)` implementation which builds an extension
//! field of `GF(2^8)`, as defined in the `galois_8` module.

use crate::galois_8;
use std::ops::{Add, Sub, Mul, Div};

// the irreducible polynomial used as a modulus for the field.
// print R.irreducible_element(2,algorithm="first_lexicographic" )
// x^2 + a*x + a^7
//
// hopefully it is a fast polynomial
const EXT_POLY: [u8; 3] = [1, 2, 128];

#[derive(Debug)]
enum EgcdRhs {
    Element(Element),
    ExtPoly,
}

/// An element of `GF(2^16)`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Element(pub [u8; 2]);

impl Element {
    /// Create the zero element.
    pub fn zero() -> Self {
        Element([0, 0])
    }

    /// A constant element evaluating to `n`.
    pub fn constant(n: u8) -> Element { Element([0, n]) }

    /// Whether this is the zero element.
    pub fn is_zero(&self) -> bool {
        self.0 == [0; 2]
    }

    // reduces from some polynomial with degree <= 2.
    #[inline]
    fn reduce_from(mut x: [u8; 3]) -> Self {
        if x[0] != 0 {
            // divide x by EXT_POLY and use remainder.
            // i = 0 here.
            // c*x^(i+j)  = a*x^i*b*x^j
            x[1] ^= galois_8::mul(EXT_POLY[1], x[0]);
            x[2] ^= galois_8::mul(EXT_POLY[2], x[0]);
        }

        Element([x[1], x[2]])
    }

    fn degree(&self) -> usize {
        if self.0[0] != 0 { 
            1 
        } else {
            0
        }
    }

    // compute extended euclidean algorithm against an element of self,
    // where the GCD is known to be constant.
    fn const_egcd(self, rhs: EgcdRhs) -> (u8, Element, Element) {
        let x = if self.is_zero() {
            let rhs = match rhs {
                EgcdRhs::Element(elem) => elem,
                EgcdRhs::ExtPoly => panic!("const_egcd invoked with divisible"),
            };
            println!("self 0. RHS = {:?}", rhs);
            (rhs.0[1], Element::constant(0), Element::constant(1))
        } else {
            let (cur_quotient, cur_remainder) = match rhs {
                EgcdRhs::Element(rhs) => rhs.polynom_div(self),
                EgcdRhs::ExtPoly => Element::div_ext_by(self),
            };

            println!("{:?} / {:?} = {:?}", rhs, self, (cur_quotient, cur_remainder));

            // GCD is constant because EXT_POLY is irreducible
            let (g, x, y) = cur_remainder.const_egcd(EgcdRhs::Element(self));
            (g, y + (cur_quotient * x), x)
        };

        println!("{:?}", x);
        x
    }

    // divide EXT_POLY by self.
    fn div_ext_by(rhs: Self) -> (Element, Element) {
        let divisor_degree = rhs.degree();

        // ensure divisor is monic.
        let leading_mul_inv = match divisor_degree {
            1 => galois_8::div(1, rhs.0[0]),
            0 => galois_8::div(1, rhs.0[1]),
            _ => panic!("GF((2^8)^2) elements can be linear at most"),
        };

        let monictized = rhs * leading_mul_inv;
        let mut poly = EXT_POLY;

        for i in 0..(3 - divisor_degree) {
            let coef = poly[i];
            for j in 1..(divisor_degree + 1) {
                if rhs.0[j] != 0 {
                    poly[i + j] ^= galois_8::mul(monictized.0[j], coef);
                }
            }
        }

        let remainder = Element([ poly[1], poly[2] ]);
        let quotient = Element::constant(poly[0]) * leading_mul_inv;

        (quotient, remainder)
    }

    fn polynom_div(self, rhs: Self) -> (Element, Element) {
        let divisor_degree = rhs.degree();
        if rhs.is_zero() {
            panic!("divide by 0");
        } else if self.degree() < divisor_degree {
            // If divisor's degree (len-1) is bigger, all dividend is a remainder
            (Element::zero(), self)
        } else if divisor_degree == 0 {
            // divide by constant.
            let invert = galois_8::div(1, rhs.0[1]);
            let quotient = Element([
                galois_8::mul(invert, self.0[0]),
                galois_8::mul(invert, self.0[1]),
            ]);

            (quotient, Element::zero())
        } else {
            // self degree is at least divisor degree, divisor degree not 0.
            // therefore both are 1. 
            debug_assert_eq!(self.degree(), divisor_degree);
            debug_assert_eq!(self.degree(), 1);

            // ensure rhs is constant.
            let leading_mul_inv = galois_8::div(1, rhs.0[0]);
            let monic = Element([
                galois_8::mul(leading_mul_inv, rhs.0[0]),
                galois_8::mul(leading_mul_inv, rhs.0[1]),
            ]);

            let leading_coeff = self.0[0];
            let mut remainder = self.0[1];

            if monic.0[1] != 0 {
                remainder ^= galois_8::mul(monic.0[1], self.0[0]);
            }

            (
                Element::constant(galois_8::mul(leading_mul_inv, leading_coeff)),
                Element::constant(remainder),
            )
        }
    }

    /// Convert the inverse of this field element. Panics if zero.
    fn inverse(self) -> Element {
        use crate::poly::Polynom;

        if !self.is_zero() {
            println!("{:?} EGCD EXT_POLY", self);

            // UNDER CONSTRUCTION: all in this file.
            {
                println!("OUR METHOD");
                // first step of extended euclidean algorithm.
                // done here because EXT_POLY is outside the scope of `Element`.
                // divide self by EXT_POLY.
                let (gcd, x, y) = {
                    let remainder = self; // EXT_POLY, self = (0, self)
                    println!("{:?} / {:?} = {:?}", self, EXT_POLY, (0, self));

                    // GCD is constant because EXT_POLY is irreducible
                    let (g, x, y) = remainder.const_egcd(EgcdRhs::ExtPoly);

                    let foo = (g, y, x);
                    println!("{:?}", foo);
                    foo
                };

                // we still need to normalize it by dividing by the gcd
                if gcd != 0 {
                    // EXT_POLY is irreducible so the GCD will always be constant.
                    // EXT_POLY*x + self*y = gcd
                    // self*y = gcd - EXT_POLY*x
                    //
                    // EXT_POLY*x is representative of the equivalence class of 0.
                    let normalizer = galois_8::div(1, gcd);
                    let _ = y * normalizer;
                }
            }

            println!("GEN METHOD");
            let rep_polynom = Polynom::from(&self.0[..]);
            let ext_poly = Polynom::from(&EXT_POLY[..]);
            
            let (gcd, _, y) = ext_poly.egcd(&rep_polynom);
            
            // // we still need to normalize it by dividing by the gcd
            // if !gcd.is_zero() {
            //     // EXT_POLY is irreducible so the GCD will always be constant.
            //     // self*x = gcd - EXT_POLY*y 
            //     // self*x/gcd = 1 - EXT_POLY*y
            //     //
            //     // EXT_POLY*y is representative of the equivalence class of 0.
            //     let normalizer = crate::galois_8::div(1, gcd[gcd.len()-1]);
            //     let normal_x = &x * normalizer;

            //     assert!(normal_x.len() <= 2);

            //     let mut coeffs = [0; 2];
            //     let poly_len = normal_x.len();
            //     coeffs[(2 - poly_len) ..][.. poly_len].copy_from_slice(&normal_x[..]);
            //     return Element(coeffs)
            // }

            // we still need to normalize it by dividing by the gcd
            if !gcd.is_zero() {
                // EXT_POLY is irreducible so the GCD will always be constant.
                // self*x = gcd - EXT_POLY*y 
                // self*x/gcd = 1 - EXT_POLY*y
                //
                // EXT_POLY*y is representative of the equivalence class of 0.
                let normalizer = crate::galois_8::div(1, gcd[gcd.len()-1]);
                let normal_y = &y * normalizer;

                assert!(normal_y.len() <= 2);

                let mut coeffs = [0; 2];
                let poly_len = normal_y.len();
                coeffs[(2 - poly_len) ..][.. poly_len].copy_from_slice(&normal_y[..]);
                return Element(coeffs)
            }
        }
        // either self is zero polynomial or is equivalent to 0
        panic!("Cannot invert 0");
    }
}

impl From<[u8; 2]> for Element {
    fn from(c: [u8; 2]) -> Self { Element(c) }
}

impl Default for Element {
    fn default() -> Self { Element::zero() }
}

impl Add for Element {
    type Output = Element;

    fn add(self, other: Self) -> Element {
        Element([
            self.0[0] ^ other.0[0],
            self.0[1] ^ other.0[1],
        ])
    }
}

impl Sub for Element {
    type Output = Element;

    fn sub(self, other: Self) -> Element {
        self.add(other)
    }
}

impl Mul for Element {
    type Output = Element;

    fn mul(self, rhs: Self) -> Element {
        // FOIL; our elements are linear at most, with two coefficients
        let out: [u8; 3] = [
            galois_8::mul(self.0[0], rhs.0[0]),
            galois_8::add(
                galois_8::mul(self.0[1], rhs.0[0]), 
                galois_8::mul(self.0[0], rhs.0[1]),
            ),
            galois_8::mul(self.0[1], rhs.0[1]),
        ];

        Element::reduce_from(out)
    }
}

impl Mul<u8> for Element {
    type Output = Element;

    fn mul(self, rhs: u8) -> Element {
        Element([
            galois_8::mul(rhs, self.0[0]),
            galois_8::mul(rhs, self.0[1]),
        ])
    }
}

impl Div for Element {
    type Output = Element;

    fn div(self, rhs: Self) -> Element {
        self * rhs.inverse()
    }
}

/// Perform exponentiation.
pub fn exp(mut elem: Element, n: usize) -> Element {
    if elem.0 == [0; 2] {
        elem
    } else if n == 0 {
        Element([0, 1])
    } else if n == 1 {
        elem
    } else {
        for _ in 1..n {
            elem = elem * elem;
        }

        elem
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Arbitrary;

    impl Arbitrary for Element {
        fn arbitrary<G: quickcheck::Gen>(gen: &mut G) -> Self {
            let a = u8::arbitrary(gen);
            let b = u8::arbitrary(gen);

            Element([a, b])
        }
    }

    quickcheck! {
        fn qc_add_associativity(a: Element, b: Element, c: Element) -> bool {
            a + (b + c) == (a + b) + c
        }

        fn qc_mul_associativity(a: Element, b: Element, c: Element) -> bool {
            a * (b * c) == (a * b) * c
        }

        fn qc_additive_identity(a: Element) -> bool {
            let zero = Element::zero();
            a - (zero - a) == zero
        }

        fn qc_multiplicative_identity(a: Element) -> bool {
            a.is_zero() || {
                let one = Element([0, 1]);
                (one / a) * a == one
            }
        }

        fn qc_add_commutativity(a: Element, b: Element) -> bool {
            a + b == b + a
        }

        fn qc_mul_commutativity(a: Element, b: Element) -> bool {
            a * b == b * a
        }

        fn qc_add_distributivity(a: Element, b: Element, c: Element) -> bool {
            a * (b + c) == (a * b) + (a * c)
        }
    }

    #[test]
    #[should_panic]
    fn test_div_b_is_0() {
        let _ = Element([1, 0]) / Element::zero();
    }
}