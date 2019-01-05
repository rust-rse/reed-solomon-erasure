//! Polynomial arithmetics over GF(256)
//!
//! adapted from https://github.com/mersinvald/reed-solomon-rs

use std::cmp::max;
use poly::Polynom;
use galois_8;

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
        let mut poly = Polynom::with_length(max(self.len(), rhs.len()));

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
