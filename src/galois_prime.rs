use std::ops::{Add, Div, Mul, MulAssign, Sub};
use ark_bls12_381::{fq, fr};

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Field;

impl crate::Field for Field {
    //todo: the order is much bigger.
    const ORDER: usize = usize::MAX;
    type Elem = fr::Fr;

    fn add(a: Self::Elem, b: Self::Elem) -> Self::Elem {
        a.add(b)
    }

    fn sub(a: Self::Elem, b: Self::Elem) -> Self::Elem {
        a.sub(b)
    }

    fn mul(a: Self::Elem, b: Self::Elem) -> Self::Elem {
        a.mul(b)
    }

    fn div(a: Self::Elem, b: Self::Elem) -> Self::Elem {
        a.div(b)
    }

    fn exp(a: Self::Elem, n: usize) -> Self::Elem {
        if n == 0 {
            return fr::Fr::from(1);
        }
        if a ==  fr::Fr::from(0) {
            return fr::Fr::from(0);
        }
        let mut r = a;
        for _ in 1..n {
            r.mul_assign(a);
        }
        r
    }

    fn zero() -> Self::Elem {
        fr::Fr::from(0)
    }

    fn one() -> Self::Elem {
        fr::Fr::from(1)
    }

    fn nth_internal(n: usize) -> Self::Elem {
        fr::Fr::from(n as u64)
    }
}

pub type ReedSolomon = crate::ReedSolomon<Field>;

pub type ShardByShard<'a> = crate::ShardByShard<'a, Field>;
