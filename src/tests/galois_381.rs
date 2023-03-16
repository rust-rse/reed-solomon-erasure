use ark_bls12_381::Fq;
use crate::galois_381::{Field, ReedSolomon};
use super::{fill_random, option_shards_into_shards, shards_into_option_shards};

#[test]
fn test() {
    let rs = ReedSolomon::new(4, 8).expect("cannot create RS_381");

    let mut shards  = Vec::with_capacity(12);
    for _ in 0..12 {
        let mut s = Vec::with_capacity(4);
        let e = Fq::from(1234);
        s.push(e);
        shards.push(s);
    }
    rs.encode(&mut shards).unwrap();
}