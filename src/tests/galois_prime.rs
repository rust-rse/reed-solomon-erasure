use std::convert::TryFrom;
use std::ops::{Div, Mul};
use ark_bls12_381::{Fr, fr};
use ark_ff::BigInteger;
use crate::galois_prime::{Field, ReedSolomon};
use crate::matrix::Matrix;
use crate::tests::{galois_prime, option_shards_to_shards, shards_to_option_shards};
use super::{fill_random, option_shards_into_shards, shards_into_option_shards};

fn print_shards(shards: &Vec<Vec<Fr>>) {
    for shard in shards {
        print!("shard: ");
        for e in shard {
            print!("{:?}",e.to_string());
        }
        println!(" ");
    }
}

fn display_matrix(mat: &Matrix<Field>) {
    for i in 0..mat.row_count() {
        for j in 0..mat.col_count() {
            print!("0{},", mat.get(i,j));
        }
        println!("");
    }
}

#[test]
fn test_fr() {
    let e = fr::Fr::from(1234);
    let one = fr::Fr::from(1);
    let i = one.div(e);

    let ii = one.div(i);
    println!("{:?}",e);
    println!("{:?}",i.clone());
    println!("{:?}",one);
    println!("{:?}",ii);

    assert_eq!(ii,e);

    let o = e.mul(i);
    assert_eq!(o,one);

    let a = fr::Fr::from(123);
    let b = fr::Fr::from(234);
    let c = fr::Fr::from(345);

    let d = a.mul(b).mul(c); // d= a*b*c
    let e = b.mul(c); // e=b*c

    let f = d.div(e); // f=d/e

    let aaa = f.0;
    println!("{:?}",aaa.to_bytes_le());

    assert_eq!(f,a); // f == a ?
}

#[test]
fn test_mat_gf() {
    let m = Matrix::<Field>::vandermonde(3,3);
    let inv = m.invert().unwrap();
    let invinv = inv.invert().unwrap();
    let mut aug = m.augment(&Matrix::<Field>::identity(3));


    println!("aug");
    display_matrix(&aug);

    aug.gaussian_elim().unwrap();
    println!("gaussian elim aug");
    display_matrix(&aug);

    println!("matrix");
    display_matrix(&m);

    println!("inv");
    display_matrix(&inv);

    println!("inv inv");
    display_matrix(&invinv);

    let mm = m.multiply(&inv);

    println!("mul");
    display_matrix(&mm);
}

#[test]
fn test() {
    let (k,r) = (4,2);
    let rs = ReedSolomon::new(k, r).expect("cannot create RS_381");

    let mut shards  = Vec::with_capacity(k+r);
    for i in 0..k+r {
        let mut s = Vec::with_capacity(1);
        let e = Fr::from(i as u32 + 1);
        s.push(e);
        shards.push(s);
    }

    print_shards(&shards);

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
        print_shards(&shards);
        assert!(rs.verify(&shards).unwrap());
        assert_eq!(&shards, &master_copy);
    }
}