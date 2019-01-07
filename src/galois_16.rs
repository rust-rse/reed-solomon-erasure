//! GF(2^16) implementation.
//!
//! More accurately, this is a `GF((2^8)^2)` implementation which builds an extension
//! field of `GF(2^8)`, as defined in the `galois_8` module.

use crate::galois_8;
use std::ops::{Add, Sub, Mul};

/// An element of `GF(2^16)`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Element(pub [u8; 2]);

impl Element {
    /// Create the zero element.
    pub fn zero() -> Self {
        Element([0, 0])
    }

    fn reduce_from(mut x: [u8; 3]) -> Self {
        // the irreducible polynomial used as a modulus for the field.
        // print R.irreducible_element(2,algorithm="first_lexicographic" )
        // x^2 + a*x + a^7
        //
        // hopefully it is a fast polynomial
        const EXT_POLY: [u8; 3] = [1, 2, 128];

        if x[0] != 0 {
            // divide x by EXT_POLY and use remainder.
            // i = 0 here.
            // c*x^(i+j)  = a*x^i*b*x^j
            x[1] ^= galois_8::mul(EXT_POLY[1], x[0]);
            x[2] ^= galois_8::mul(EXT_POLY[2], x[0]);
        }

        Element([x[1], x[2]])
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
        // FOIL; our elements are quadratic at most.
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
}