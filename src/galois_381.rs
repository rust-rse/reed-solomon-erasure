use std::ops::{Add, Div, Mul, MulAssign};
use ark_bls12_381::fq;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Field;

impl crate::Field for Field {
    //todo: the order is much bigger.
    const ORDER: usize = usize::MAX;
    type Elem = fq::Fq;

    fn add(a: Self::Elem, b: Self::Elem) -> Self::Elem {
        a.add(b)
    }

    fn mul(a: Self::Elem, b: Self::Elem) -> Self::Elem {
        a.mul(b)
    }

    fn div(a: Self::Elem, b: Self::Elem) -> Self::Elem {
        a.div(b)
    }

    fn exp(a: Self::Elem, n: usize) -> Self::Elem {
        if n == 0 {
            return fq::FQ_ONE
        }
        if a ==  fq::FQ_ZERO {
            return fq::FQ_ONE
        }
        let mut r = a;
        for _ in 1..n {
            r.mul_assign(a);
        }
        r
    }

    fn zero() -> Self::Elem {
        fq::FQ_ZERO
    }

    fn one() -> Self::Elem {
        fq::FQ_ONE
    }

    fn nth_internal(n: usize) -> Self::Elem {
        fq::Fq::from(n as u64)
    }
}

pub type ReedSolomon = crate::ReedSolomon<Field>;

pub type ShardByShard<'a> = crate::ShardByShard<'a, Field>;
