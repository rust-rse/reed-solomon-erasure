use ark_bls12_381::Fq;
use crate::galois_381::{Field, ReedSolomon};
use crate::tests::{option_shards_to_shards, shards_to_option_shards};
use super::{fill_random, option_shards_into_shards, shards_into_option_shards};

#[test]
fn test() {
    let rs = ReedSolomon::new(4, 8).expect("cannot create RS_381");

    let mut shards  = Vec::with_capacity(12);
    for _ in 0..12 {
        let mut s = Vec::with_capacity(1);
        let e = Fq::from(1234);
        s.push(e);
        shards.push(s);
    }
    rs.encode(&mut shards).unwrap();
    assert!(rs.verify(&shards).unwrap());

    let master_copy = shards.clone();

    let mut shards = shards_to_option_shards(&shards);

    rs.reconstruct(&mut shards).unwrap();
    {
        let shards = option_shards_to_shards(&shards);
        assert!(rs.verify(&shards).unwrap());
        assert_eq!(&shards, &master_copy);
    }
    shards[0] = None;
    shards[2] = None;
    //shards[4] = None;
    rs.reconstruct(&mut shards).unwrap();
    {
        let shards = option_shards_to_shards(&shards);
        assert!(rs.verify(&shards).unwrap());
        assert_eq!(&shards, &master_copy);
    }
}