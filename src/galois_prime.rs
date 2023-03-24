use std::ops::{Add, Div, Mul, MulAssign, Sub};
use ark_bls12_381::{fq, fr};
use ark_ff::fields::Field as OtherField;
use ark_ff::{BigInt, BigInteger, PrimeField};

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

    fn slice_to_vec(input: &[Self::Elem]) -> Vec<u8> {
        input.iter()
            //.flat_map(|&u| u.into_bigint().to_bytes_le())
            .flat_map(|&u|  {
                //let nb_bytes = (u.into_bigint().num_bits()+7)/8;
                let mut v = u.into_bigint().to_bytes_le();
                v.truncate((fr::Fr::MODULUS_BIT_SIZE as usize) / 8 as usize);
                v
            })
            .collect()
    }

    fn from_vec(input: Vec<u8>) -> Vec<fr::Fr> {
        let mut output = Vec::new();

        let chunks = input.chunks((fr::Fr::MODULUS_BIT_SIZE as usize) / 8);
        for chunk  in chunks {
            //output.push(fr::Fr::from_random_bytes(chunk).unwrap());
            output.push(fr::Fr::from_le_bytes_mod_order(chunk));
        }

        output
    }
}

pub type ReedSolomon = crate::ReedSolomon<Field>;

pub type ShardByShard<'a> = crate::ShardByShard<'a, Field>;
