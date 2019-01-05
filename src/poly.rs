//! Polynomial over GF(256)
//!
//! adapted from https://github.com/mersinvald/reed-solomon-rs

use std::cmp;
use std::ops::{Deref, DerefMut};
use std::fmt;
use galois_8;

const POLYNOMIAL_MAX_LENGTH: usize = 256;

#[derive(Copy)]
pub struct Polynom {
    array: [u8; POLYNOMIAL_MAX_LENGTH],
    length: usize,
    dirty: bool,
}

impl Polynom {
    #[inline]
    pub fn new() -> Polynom {
        Polynom {
            array: [0; POLYNOMIAL_MAX_LENGTH],
            length: 0,
            dirty: false,
        }
    }

    #[inline]
    pub fn with_length(len: usize) -> Polynom {
        let mut p = Polynom::new();
        p.length = len;
        p
    }

    #[inline]
    pub fn set_length(&mut self, new_len: usize) {
        let old_len = self.len();
        self.length = new_len;
        
        if self.dirty && new_len > old_len {
            for x in self.iter_mut().skip(old_len)
                                    .take(new_len - old_len) 
            {
                *x = 0;
            }
        } else if new_len < old_len {
            self.dirty = true;
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.length
    }

    #[inline]
    pub fn is_zero(&self) -> bool {
        (self.length == 1) && (self.array[0] == 0)
    }

    #[inline]
    pub fn reverse(mut self) -> Self {
        (*self).reverse();
        self
    }

    #[inline]
    pub fn push(&mut self, x: u8) {
        self.array[self.length] = x;
        self.length += 1;
    }
}

impl Clone for Polynom {
    #[inline]
    fn clone(&self) -> Polynom {
        *self
    }
}

impl Default for Polynom {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for Polynom {
    type Target = [u8];
    #[inline]
    fn deref(&self) -> &Self::Target {
        let len = self.len();
        &self.array[0..len]
    }
}

impl DerefMut for Polynom {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        let len = self.len();
        &mut self.array[0..len]
    }
}

impl<'a> From<&'a [u8]> for Polynom {
    #[inline]
    fn from(slice: &'a [u8]) -> Polynom {
        debug_assert!(slice.len() <= POLYNOMIAL_MAX_LENGTH);
        let mut poly = Polynom::with_length(slice.len());
        poly[..].copy_from_slice(slice);
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
        if self.length == other.length && !self.dirty && !other.dirty {
            self.array.iter().zip(other.array.iter()).all(|(a,b)| a == b)
        } else {
            let mut self_first_sig_index = 0;
            let mut other_first_sig_index = 0;
            for i in 0..(self.len()) {
                if self[i] == 0 {
                    self_first_sig_index += 1;
                } else {
                    break;
                }
            }
            for i in 0..(other.len()) {
                if other[i] == 0 {
                    other_first_sig_index += 1;
                } else {
                    break;
                }
            }
            if (self.len() - self_first_sig_index) != (other.len() - other_first_sig_index) {
                return false;
            }
            for i in 0..(self.len()-self_first_sig_index) {
                if self[i+self_first_sig_index] != other[i+other_first_sig_index] {
                    return false;
                }
            }
            return true;
        }
    }
}

pub trait Scale {
    fn scale(&self, x: u8) -> Polynom;
    fn scale_assign(&mut self, x: u8) -> &mut Self;
}

pub trait Add {
    fn add(&self, rhs: &Self) -> Polynom;
    fn add_assign(&mut self, rhs: &Self) -> &mut Self;
}

pub trait Mul {
    fn mul(&self, rhs: &Self) -> Polynom;
}

pub trait Div {
    fn div(&self, rhs: &Self) -> (Polynom, Polynom);
}

pub trait EGCD {
    fn egcd(&self, rhs: &Self) -> (Polynom, Polynom, Polynom);
}


pub trait Eval {
    fn eval(&self, x: u8) -> u8;
}

impl Scale for [u8] {
    #[inline]
    fn scale(&self, x: u8) -> Polynom {
        let mut poly = Polynom::from(self);
        poly.scale_assign(x);
        poly
    }

    #[inline]
    fn scale_assign(&mut self, x: u8) -> &mut Self {
        for px in self.iter_mut() {
            *px = galois_8::mul(*px, x);
        }
        self
    }
}

impl Add for [u8] {
    fn add(&self, rhs: &Self) -> Polynom {
        let mut poly = Polynom::with_length(cmp::max(self.len(), rhs.len()));

        for (i, x) in self.iter().enumerate() {
            let index = i + poly.len() - self.len();
            poly[index] = *x;
        }

        for (i, x) in rhs.iter().enumerate() {
            let index = i + poly.len() - rhs.len();
            poly[index] ^= *x;
        }

        poly
    }

    fn add_assign(&mut self, rhs: &Self) -> &mut Self {
        let poly = self.add(rhs);
        self.copy_from_slice(&poly);
        self
    }
}

impl Mul for [u8] {
    #[inline]
    fn mul(&self, rhs: &Self) -> Polynom {
        let mut poly = Polynom::with_length(self.len() + rhs.len() - 1);

        for (j, rhs_x) in rhs.iter().enumerate() {
            for (i, self_x) in self.iter().enumerate() {
                poly[i + j] ^= galois_8::mul(*self_x, *rhs_x);
            }
        }

        poly    }

}

pub trait AuxFunctions {
    fn is_zero(self: &Self) -> bool;
}

impl AuxFunctions for [u8] {
    fn is_zero(self: &[u8]) -> bool {
        self.is_empty() || self.len() == 1 && self[0] == 0
    }
}

impl Div for [u8] {
    fn div(&self, rhs: &Self) -> (Polynom, Polynom) {
        if rhs.is_zero() {
            panic!("Divisor is 0")
        }
            
        let mut poly = Polynom::from(self);

        // If divisor's degree (len-1) is bigger, all dividend is a remainder
        let divisor_degree = rhs.len() - 1;
        if self.len() < divisor_degree {
            return (Polynom::new(), poly);
        }
        //TODO: by shifting rhs[0] make sure the leading coefficient rhs[0] isn't 0
        let monictized = rhs.scale(galois_8::div(1,rhs[0]));

        for i in 0..(self.len() - divisor_degree) {
            let coef =  poly[i];
            if coef != 0 {
                for j in 1..monictized.len() {
                    if rhs[j] != 0 {
                        poly[i + j] ^= galois_8::mul(monictized[j], coef); // c*x^(i+j)  = a*x^i*b*x^j
                    }
                }
            }
        }

        let separator = self.len() - (rhs.len() - 1);

        // Quotient is after separator
        let remainder = Polynom::from(&poly[separator..]);

        // And reminder is before separator, so just shrink to it
        poly.set_length(separator);
        poly.scale_assign(galois_8::div(1,rhs[0]));

        (poly, remainder)
    }
}


impl EGCD for [u8] {
    fn egcd(&self, rhs: &Self) -> (Polynom, Polynom, Polynom) {
        
        if self.is_zero() {
            (Polynom::from(rhs), polynom![0], polynom![1])
        } else {
            let (cur_quotient, cur_remainder) = rhs.div(self);
            let (g, x, y) = cur_remainder.egcd(self);
            (g, Polynom::from(y.add(&cur_quotient.mul(&x))), Polynom::from(x))
        }
    }
}
    

impl Eval for [u8] {
    #[inline]
    fn eval(&self, x: u8) -> u8 {
        let mut y = self[0];
        for px in self.iter().skip(1) {
            y = galois_8::mul(y, x) ^ px;
        }
        y
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push() {
        let mut poly = polynom![];
        for i in 0..10 {
            poly.push(i);
            for j in 0..(i as usize) {
                assert!(poly[j] == j as u8);
            }
        }
    }

    #[test]
    fn reverse() {
        let poly = polynom![5, 4, 3, 2, 1, 0];
        for (i, x) in poly.reverse().iter().enumerate() {
            assert_eq!(i, *x as usize);
        }
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
        let poly = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let answer = [0, 3, 6, 5, 12, 15, 10, 9, 24, 27];
        assert_eq!(answer, *(poly.scale(3)));
    }

    #[test]
    fn scale_assign() {
        let mut poly = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let answer = [0, 3, 6, 5, 12, 15, 10, 9, 24, 27];
        assert_eq!(answer,
                   *({
                       poly.scale_assign(3);
                       &poly
                   }));
    }

    #[test]
    fn add() {
        let px = [0, 5, 10, 15, 20];
        let py = [3, 9, 17, 24, 75];
        assert_eq!([3, 12, 27, 23, 95], *(px.add(&py)));

        let px = [0, 5, 10];
        let py = [3, 9, 17, 24, 75];

        assert_eq!([3, 9, 17, 29, 65], *(px.add(&py)));
        assert_eq!([3, 9, 17, 29, 65], *(py.add(&px)))
    }

    #[test]
    fn mul() {
        let px = [0, 5, 10, 15, 20];
        let py = [3, 9, 17, 24, 75];
        assert_eq!([0, 15, 51, 30, 153, 193, 53, 115, 245], *(px.mul(&py)));

        let px = [0, 5, 10];
        let py = [3, 9, 17, 24, 75];

        assert_eq!([0, 15, 51, 15, 210, 138, 244], *(px.mul(&py)));
        assert_eq!([0, 15, 51, 15, 210, 138, 244], *(py.mul(&px)));
    }


    #[test]
    fn div() {
        let px = [0, 5, 10, 15, 20];
        let py = [3, 9, 17, 24, 75];

        let (q, r) = px.div(&py);
        assert_eq!([0], *q);
        assert_eq!([5, 10, 15, 20], *r);

        let (q, r) = py.div(&px);
        assert_eq!([3], *q);
        assert_eq!([6, 15, 9, 119], *r);

        let px = [0, 5, 10];
        let py = [3, 9, 17, 24, 75];

        let empty: [u8; 0] = [];
        let (q, r) = px.div(&py);

        assert_eq!(empty, *q);
        assert_eq!([0, 5, 10], *r);

        let (q, r) = py.div(&px);
        assert_eq!([3, 6, 17], *q);
        assert_eq!([113, 225], *r);
    }

    #[test]
    fn eval() {
        let p = [0, 5, 10, 15, 20];
        let tests = [4, 7, 21, 87, 35, 255];
        let answers = [213, 97, 132, 183, 244, 92];

        for i in 0..tests.len() {
            assert_eq!(answers[i], p.eval(tests[i]));
        }
    }
}
