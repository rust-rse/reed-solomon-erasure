#![cfg(test)]
#![allow(dead_code)]

use super::*;
use shard_utils;
use rand::{thread_rng, Rng};

macro_rules! shards {
    (
        $( [ $( $x:expr ),* ] ),*
    ) => {{
        vec![ $( vec![ $( $x as u8),* ].into_boxed_slice() ),* ]
    }}
}

macro_rules! make_random_shards {
    ($per_shard:expr, $size:expr) => {{
        let mut shards = Vec::with_capacity(20);
        for _ in 0..$size {
            shards.push(make_blank_shard($per_shard));
        }

        for s in shards.iter_mut() {
            fill_random(s);
        }

        shards
    }}
}

fn assert_eq_shards(s1 : &[Shard], s2 : &[Shard]) {
    assert_eq!(s1.len(), s2.len());
    for i in 0..s1.len() {
        assert_eq!(s1[i],
                   s2[i]);
    }
}

fn fill_random(arr : &mut [u8]) {
    for a in arr.iter_mut() {
        *a = rand::random::<u8>();
    }
}

#[test]
fn test_no_data_shards() {
    assert_eq!(Error::TooFewDataShards, ReedSolomon::new(0, 1).unwrap_err()); }

#[test]
fn test_no_parity_shards() {
    assert_eq!(Error::TooFewParityShards, ReedSolomon::new(1, 0).unwrap_err()); }

#[test]
fn test_too_many_shards() {
    assert_eq!(Error::TooManyShards, ReedSolomon::new(129, 128).unwrap_err()); }

#[test]
fn test_shard_count() {
    let mut rng = thread_rng();
    for _ in 0..10 {
        let data_shard_count   = rng.gen_range(1, 128);
        let parity_shard_count = rng.gen_range(1, 128);

        let total_shard_count = data_shard_count + parity_shard_count;

        let r = ReedSolomon::new(data_shard_count, parity_shard_count).unwrap();

        assert_eq!(data_shard_count,   r.data_shard_count());
        assert_eq!(parity_shard_count, r.parity_shard_count());
        assert_eq!(total_shard_count,  r.total_shard_count());
    }
}

#[test]
fn test_shards_into_option_shards_into_shards() {
    for _ in 0..100 {
        let shards = make_random_shards!(1_000, 10);
        let expect = shards.clone();
        let inter  =
            shard_utils::shards_into_option_shards(shards);
        let result =
            shard_utils::option_shards_into_shards(inter);

        assert_eq_shards(&expect, &result);
    }
}

#[test]
fn test_shards_to_option_shards_to_shards() {
    for _ in 0..100 {
        let shards = make_random_shards!(1_000, 10);
        let expect = shards.clone();
        let option_shards =
            shard_utils::shards_to_option_shards(&shards);
        let result        =
            shard_utils::option_shards_to_shards(&option_shards);

        assert_eq_shards(&expect, &result);
    }
}

#[test]
#[should_panic]
fn test_option_shards_to_shards_missing_shards_case1() {
    let shards = make_random_shards!(1_000, 10);
    let mut option_shards = shards_into_option_shards(shards);

    option_shards[0] = None;

    shard_utils::option_shards_to_shards(&option_shards);
}

#[test]
fn test_option_shards_to_shards_missing_shards_case2() {
    let shards = make_random_shards!(1_000, 10);
    let mut option_shards = shards_into_option_shards(shards);

    option_shards[0] = None;
    option_shards[9] = None;

    shard_utils::option_shards_to_shards(&option_shards[1..8]);
}

#[test]
#[should_panic]
fn test_option_shards_into_missing_shards() {
    let shards = make_random_shards!(1_000, 10);
    let mut option_shards = shards_into_option_shards(shards);

    option_shards[2] = None;

    shard_utils::option_shards_into_shards(option_shards);
}

#[test]
#[should_panic]
fn test_option_shards_to_shards_too_few_shards() {
    let shards = make_random_shards!(1_000, 10);
    let option_shards = shards_into_option_shards(shards);

    shard_utils::option_shards_to_shards(&option_shards[0..11]);
}

#[test]
fn test_reedsolomon_clone() {
    let r1 = ReedSolomon::new(10, 3).unwrap();
    let r2 = r1.clone();

    assert_eq!(r1, r2);
}

#[test]
fn test_encoding() {
    let per_shard = 50_000;

    let r = ReedSolomon::new(10, 3).unwrap();

    let mut shards = make_random_shards!(per_shard, 13);

    r.encode_shards(&mut shards).unwrap();
    assert!(r.verify_shards(&shards).unwrap());

    assert_eq!(Error::TooFewShards,
               r.encode_shards(&mut shards[0..1]).unwrap_err());

    let mut bad_shards = make_random_shards!(per_shard, 13);
    bad_shards[0] = vec![0 as u8].into_boxed_slice();
    assert_eq!(Error::IncorrectShardSize,
               r.encode_shards(&mut bad_shards).unwrap_err());
}

#[test]
fn test_reconstruct_shards() {
    let per_shard = 100_000;

    let r = ReedSolomon::new(8, 5).unwrap();

    let mut shards = make_random_shards!(per_shard, 13);

    r.encode_shards(&mut shards).unwrap();

    let master_copy = shards.clone();

    let mut shards = shards_to_option_shards(&shards);

    // Try to decode with all shards present
    r.reconstruct_shards(&mut shards).unwrap();
    {
        let shards = option_shards_to_shards(&shards);
        assert!(r.verify_shards(&shards).unwrap());
        assert_eq_shards(&shards, &master_copy);
    }

    // Try to decode with 10 shards
    shards[0] = None;
    shards[2] = None;
    //shards[4] = None;
    r.reconstruct_shards(&mut shards).unwrap();
    {
        let shards = option_shards_to_shards(&shards);
        assert!(r.verify_shards(&shards).unwrap());
        assert_eq_shards(&shards, &master_copy);
    }

    // Try to decode the same shards again to try to
    // trigger the usage of cached decode matrix
    shards[0] = None;
    shards[2] = None;
    //shards[4] = None;
    r.reconstruct_shards(&mut shards).unwrap();
    {
        let shards = option_shards_to_shards(&shards);
        assert!(r.verify_shards(&shards).unwrap());
        assert_eq_shards(&shards, &master_copy);
    }

    // Try to deocde with 6 data and 4 parity shards
    shards[0] = None;
    shards[2] = None;
    shards[12] = None;
    r.reconstruct_shards(&mut shards).unwrap();
    {
        let shards = option_shards_to_shards(&shards);
        assert!(r.verify_shards(&shards).unwrap());
        assert_eq_shards(&shards, &master_copy);
    }

    // Try to reconstruct data only
    shards[0] = None;
    shards[1] = None;
    shards[12] = None;
    r.reconstruct_data_shards(&mut shards).unwrap();
    {
        let data_shards = option_shards_to_shards(&shards[0..8]);
        assert_eq!(master_copy[0], data_shards[0]);
        assert_eq!(master_copy[1], data_shards[1]);
        assert_eq!(None, shards[12]);
    }

    // Try to decode with 7 data and 1 parity shards
    shards[0] = None;
    shards[1] = None;
    shards[9] = None;
    shards[10] = None;
    shards[11] = None;
    shards[12] = None;
    assert_eq!(r.reconstruct_shards(&mut shards).unwrap_err(),
               Error::TooFewShardsPresent);
}

#[test]
fn test_reconstruct() {
    let r = ReedSolomon::new(2, 2).unwrap();

    let mut shards : [[u8; 3]; 4] = [[0, 1, 2],
                                     [3, 4, 5],
                                     [200, 201, 203],
                                     [100, 101, 102]];

    {
        {
            let mut shard_refs : Vec<&mut [u8]> =
                Vec::with_capacity(3);

            for shard in shards.iter_mut() {
                shard_refs.push(shard);
            }

            r.encode(&mut shard_refs).unwrap();
        }

        let shard_refs =
            convert_2D_slices!(shards =>to_vec &[u8]);

        assert!(r.verify(&shard_refs).unwrap());
    }

    {
        {
            let mut shard_refs =
                convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

            shard_refs[0][0] = 101;
            shard_refs[0][1] = 102;
            shard_refs[0][2] = 103;

            let shards_present = [false, true, true, true];

            r.reconstruct(&mut shard_refs, &shards_present).unwrap();
        }

        let shard_refs =
            convert_2D_slices!(shards =>to_vec &[u8]);

        assert!(r.verify(&shard_refs).unwrap());
    }

    let expect : [[u8; 3]; 4] = [[0, 1, 2],
                                 [3, 4, 5],
                                 [6, 11, 12],
                                 [5, 14, 11]];
    assert_eq!(expect, shards);

    {
        {
            let mut shard_refs =
                convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

            shard_refs[0][0] = 201;
            shard_refs[0][1] = 202;
            shard_refs[0][2] = 203;

            shard_refs[2][0] = 101;
            shard_refs[2][1] = 102;
            shard_refs[2][2] = 103;

            let shards_present = [false, true, false, true];

            r.reconstruct_data(&mut shard_refs,
                               &shards_present).unwrap();
        }

        let shard_refs =
            convert_2D_slices!(shards =>to_vec &[u8]);

        assert!(!r.verify(&shard_refs).unwrap());
    }

    let expect : [[u8; 3]; 4] = [[0, 1, 2],
                                 [3, 4, 5],
                                 [101, 102, 103],
                                 [5, 14, 11]];
    assert_eq!(expect, shards);

    {
        {
            let mut shard_refs =
                convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

            shard_refs[2][0] = 101;
            shard_refs[2][1] = 102;
            shard_refs[2][2] = 103;

            shard_refs[3][0] = 201;
            shard_refs[3][1] = 202;
            shard_refs[3][2] = 203;

            let shards_present = [true, true, false, false];

            r.reconstruct_data(&mut shard_refs,
                               &shards_present).unwrap();
        }

        let shard_refs =
            convert_2D_slices!(shards =>to_vec &[u8]);

        assert!(!r.verify(&shard_refs).unwrap());
    }

    let expect : [[u8; 3]; 4] = [[0, 1, 2],
                                 [3, 4, 5],
                                 [101, 102, 103],
                                 [201, 202, 203]];
    assert_eq!(expect, shards);
}

quickcheck! {
    fn qc_encode_verify_reconstruct_verify(data    : usize,
                                           parity  : usize,
                                           corrupt : usize,
                                           size    : usize) -> bool {
        let data   = 1 + data % 256;
        let mut parity = 1 + parity % 256;
        if data + parity > 256 {
            parity -= data + parity - 256;
        }

        let corrupt = corrupt % (parity + 1);

        let mut corrupt_pos_s = Vec::with_capacity(corrupt);
        for _ in 0..corrupt {
            let mut pos = rand::random::<usize>() % (data + parity);

            while let Some(_) = corrupt_pos_s.iter().find(|&&x| x == pos) {
                pos = rand::random::<usize>() % (data + parity);
            }

            corrupt_pos_s.push(pos);
        }

        let size = 1 + size % 1_000_000;

        let r = ReedSolomon::new(data, parity).unwrap();

        let mut expect = make_random_shards!(size, data + parity);
        {
            let mut refs =
                convert_2D_slices!(expect =>to_mut_vec &mut [u8]);

            r.encode(&mut refs).unwrap();
        }

        let expect = expect;

        let mut shards = expect.clone();

        // corrupt shards
        for &p in corrupt_pos_s.iter() {
            fill_random(&mut shards[p]);
        }
        let mut slice_present = vec![true; data + parity];
        for &p in corrupt_pos_s.iter() {
            slice_present[p] = false;
        }

        // reconstruct
        {
            let mut refs =
                convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

            r.reconstruct(&mut refs, &slice_present).unwrap();
        }

        ({
            let refs =
                convert_2D_slices!(expect =>to_vec &[u8]);

            r.verify(&refs).unwrap()
        })
            &&
            expect == shards
            &&
            ({
                let refs =
                    convert_2D_slices!(shards =>to_vec &[u8]);

                r.verify(&refs).unwrap()
            })
    }

    fn qc_encode_verify_reconstruct_verify_shards(data    : usize,
                                                  parity  : usize,
                                                  corrupt : usize,
                                                  size    : usize) -> bool {
        let data   = 1 + data % 256;
        let mut parity = 1 + parity % 256;
        if data + parity > 256 {
            parity -= data + parity - 256;
        }

        let corrupt = corrupt % (parity + 1);

        let mut corrupt_pos_s = Vec::with_capacity(corrupt);
        for _ in 0..corrupt {
            let mut pos = rand::random::<usize>() % (data + parity);

            while let Some(_) = corrupt_pos_s.iter().find(|&&x| x == pos) {
                pos = rand::random::<usize>() % (data + parity);
            }

            corrupt_pos_s.push(pos);
        }

        let size = 1 + size % 1_000_000;

        let r = ReedSolomon::new(data, parity).unwrap();

        let mut expect = make_random_shards!(size, data + parity);
        r.encode_shards(&mut expect).unwrap();

        let expect = expect;

        let mut shards = shards_into_option_shards(expect.clone());

        // corrupt shards
        for &p in corrupt_pos_s.iter() {
            shards[p] = None;
        }

        // reconstruct
        r.reconstruct_shards(&mut shards).unwrap();

        let shards = option_shards_into_shards(shards);

        r.verify_shards(&expect).unwrap()
            && expect == shards
            && r.verify_shards(&shards).unwrap()
    }

    fn qc_verify(data    : usize,
                 parity  : usize,
                 corrupt : usize,
                 size    : usize) -> bool {
        let data   = 1 + data % 256;
        let mut parity = 1 + parity % 256;
        if data + parity > 256 {
            parity -= data + parity - 256;
        }

        let corrupt = corrupt % (parity + 1);

        let mut corrupt_pos_s = Vec::with_capacity(corrupt);
        for _ in 0..corrupt {
            let mut pos = rand::random::<usize>() % (data + parity);

            while let Some(_) = corrupt_pos_s.iter().find(|&&x| x == pos) {
                pos = rand::random::<usize>() % (data + parity);
            }

            corrupt_pos_s.push(pos);
        }

        let size = 1 + size % 1_000_000;

        let r = ReedSolomon::new(data, parity).unwrap();

        let mut expect = make_random_shards!(size, data + parity);
        {
            let mut refs =
                convert_2D_slices!(expect =>to_mut_vec &mut [u8]);

            r.encode(&mut refs).unwrap();
        }

        let expect = expect;

        let mut shards = expect.clone();

        // corrupt shards
        for &p in corrupt_pos_s.iter() {
            fill_random(&mut shards[p]);
        }

        ({
            let refs =
                convert_2D_slices!(expect =>to_vec &[u8]);

            r.verify(&refs).unwrap()
        })
            &&
            ((corrupt > 0 && expect != shards)
             || (corrupt == 0 && expect == shards))
            &&
            ({
                let refs =
                    convert_2D_slices!(shards =>to_vec &[u8]);

                (corrupt > 0 && !r.verify(&refs).unwrap())
                    || (corrupt == 0 && r.verify(&refs).unwrap())
            })
    }

    fn qc_verify_shards(data    : usize,
                        parity  : usize,
                        corrupt : usize,
                        size    : usize) -> bool {
        let data   = 1 + data % 256;
        let mut parity = 1 + parity % 256;
        if data + parity > 256 {
            parity -= data + parity - 256;
        }

        let corrupt = corrupt % (parity + 1);

        let mut corrupt_pos_s = Vec::with_capacity(corrupt);
        for _ in 0..corrupt {
            let mut pos = rand::random::<usize>() % (data + parity);

            while let Some(_) = corrupt_pos_s.iter().find(|&&x| x == pos) {
                pos = rand::random::<usize>() % (data + parity);
            }

            corrupt_pos_s.push(pos);
        }

        let size = 1 + size % 1_000_000;

        let r = ReedSolomon::new(data, parity).unwrap();

        let mut expect = make_random_shards!(size, data + parity);
        r.encode_shards(&mut expect).unwrap();

        let expect = expect;

        let mut shards = expect.clone();

        // corrupt shards
        for &p in corrupt_pos_s.iter() {
            fill_random(&mut shards[p]);
        }

        r.verify_shards(&expect).unwrap()
            &&
            ((corrupt > 0 && expect != shards)
             || (corrupt == 0 && expect == shards))
            &&
            ((corrupt > 0 && !r.verify_shards(&shards).unwrap())
             || (corrupt == 0 && r.verify_shards(&shards).unwrap()))
    }

    fn qc_encode_sep_same_as_encode(data    : usize,
                                    parity  : usize,
                                    size    : usize) -> bool {
        let data   = 1 + data % 256;
        let mut parity = 1 + parity % 256;
        if data + parity > 256 {
            parity -= data + parity - 256;
        }

        let size = 1 + size % 1_000_000;

        let r = ReedSolomon::new(data, parity).unwrap();

        let mut expect = make_random_shards!(size, data + parity);
        let mut shards = expect.clone();

        {
            let mut refs =
                convert_2D_slices!(expect =>to_mut_vec &mut [u8]);

            r.encode(&mut refs).unwrap();
        }

        let expect = expect;

        {
            let (data, parity) = shards.split_at_mut(data);

            let data_refs =
                convert_2D_slices!(data =>to_mut_vec &[u8]);

            let mut parity_refs =
                convert_2D_slices!(parity =>to_mut_vec &mut [u8]);

            r.encode_sep(&data_refs, &mut parity_refs).unwrap();
        }

        let shards = shards;

        expect == shards
    }

    fn qc_encode_sep_same_as_encode_shards(data    : usize,
                                           parity  : usize,
                                           size    : usize) -> bool {
        let data   = 1 + data % 256;
        let mut parity = 1 + parity % 256;
        if data + parity > 256 {
            parity -= data + parity - 256;
        }

        let size = 1 + size % 1_000_000;

        let r = ReedSolomon::new(data, parity).unwrap();

        let mut expect = make_random_shards!(size, data + parity);
        let mut shards = expect.clone();

        r.encode_shards(&mut expect).unwrap();

        let expect = expect;

        {
            let (data, parity) = shards.split_at_mut(data);

            r.encode_shards_sep(data, parity).unwrap();
        }

        let shards = shards;

        expect == shards
    }

    fn qc_encode_single_same_as_encode(data    : usize,
                                       parity  : usize,
                                       size    : usize) -> bool {
        let data   = 1 + data % 256;
        let mut parity = 1 + parity % 256;
        if data + parity > 256 {
            parity -= data + parity - 256;
        }

        let size = 1 + size % 1_000_000;

        let r = ReedSolomon::new(data, parity).unwrap();

        let mut expect = make_random_shards!(size, data + parity);
        let mut shards = expect.clone();

        {
            let mut refs =
                convert_2D_slices!(expect =>to_mut_vec &mut [u8]);

            r.encode(&mut refs).unwrap();
        }

        let expect = expect;

        {
            let mut refs =
                convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

            for i in 0..data {
                r.encode_single(i, &mut refs).unwrap();
            }
        }

        let shards = shards;

        expect == shards
    }

    fn qc_encode_single_same_as_encode_shards(data    : usize,
                                              parity  : usize,
                                              size    : usize) -> bool {
        let data   = 1 + data % 256;
        let mut parity = 1 + parity % 256;
        if data + parity > 256 {
            parity -= data + parity - 256;
        }

        let size = 1 + size % 1_000_000;

        let r = ReedSolomon::new(data, parity).unwrap();

        let mut expect = make_random_shards!(size, data + parity);
        let mut shards = expect.clone();

        r.encode_shards(&mut expect).unwrap();

        let expect = expect;

        for i in 0..data {
            r.encode_single_shard(i, &mut shards).unwrap();
        }

        let shards = shards;

        expect == shards
    }

    fn qc_encode_single_sep_same_as_encode(data    : usize,
                                           parity  : usize,
                                           size    : usize) -> bool {
        let data   = 1 + data % 256;
        let mut parity = 1 + parity % 256;
        if data + parity > 256 {
            parity -= data + parity - 256;
        }

        let size = 1 + size % 1_000_000;

        let r = ReedSolomon::new(data, parity).unwrap();

        let mut expect = make_random_shards!(size, data + parity);
        let mut shards = expect.clone();

        {
            let mut refs =
                convert_2D_slices!(expect =>to_mut_vec &mut [u8]);

            r.encode(&mut refs).unwrap();
        }

        let expect = expect;

        {
            let (data_shards, parity_shards) = shards.split_at_mut(data);

            let data_refs =
                convert_2D_slices!(data_shards =>to_mut_vec &[u8]);

            let mut parity_refs =
                convert_2D_slices!(parity_shards =>to_mut_vec &mut [u8]);

            for i in 0..data {
                r.encode_single_sep(i, data_refs[i], &mut parity_refs).unwrap();
            }
        }

        let shards = shards;

        expect == shards
    }

    fn qc_encode_single_sep_same_as_encode_shards(data    : usize,
                                                  parity  : usize,
                                                  size    : usize) -> bool {
        let data   = 1 + data % 256;
        let mut parity = 1 + parity % 256;
        if data + parity > 256 {
            parity -= data + parity - 256;
        }

        let size = 1 + size % 1_000_000;

        let r = ReedSolomon::new(data, parity).unwrap();

        let mut expect = make_random_shards!(size, data + parity);
        let mut shards = expect.clone();

        r.encode_shards(&mut expect).unwrap();

        let expect = expect;

        {
            let (data_shards, parity_shards) = shards.split_at_mut(data);

            for i in 0..data {
                r.encode_single_shard_sep(i, &data_shards[i], parity_shards).unwrap();
            }
        }

        let shards = shards;

        expect == shards
    }
}

#[test]
fn test_reconstruct_error_handling() {
    let r = ReedSolomon::new(2, 2).unwrap();

    let mut shards : [[u8; 3]; 4] = [[0, 1, 2],
                                     [3, 4, 5],
                                     [200, 201, 203],
                                     [100, 101, 102]];

    {
        let mut shard_refs : Vec<&mut [u8]> =
            Vec::with_capacity(3);

        for shard in shards.iter_mut() {
            shard_refs.push(shard);
        }

        r.encode(&mut shard_refs).unwrap();
    }

    {
        let mut shard_refs =
            convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

        shard_refs[0][0] = 101;
        shard_refs[0][1] = 102;
        shard_refs[0][2] = 103;

        let shards_present = [false, true, true, true, true];
        assert_eq!(Error::InvalidShardFlags,
                   r.reconstruct(&mut shard_refs, &shards_present).unwrap_err());

        let shards_present = [false, true, true];
        assert_eq!(Error::InvalidShardFlags,
                   r.reconstruct(&mut shard_refs, &shards_present).unwrap_err());

        let shards_present = [true, false, false, false];
        assert_eq!(Error::TooFewShardsPresent,
                   r.reconstruct(&mut shard_refs, &shards_present).unwrap_err());

        let shards_present = [true, false, false, true];
        r.reconstruct(&mut shard_refs, &shards_present).unwrap();
    }
}

#[test]
fn test_one_encode() {
    let r = ReedSolomon::new(5, 5).unwrap();

    let mut shards = shards!([0, 1],
                             [4, 5],
                             [2, 3],
                             [6, 7],
                             [8, 9],
                             [0, 0],
                             [0, 0],
                             [0, 0],
                             [0, 0],
                             [0, 0]);

    r.encode_shards(&mut shards).unwrap();
    { assert_eq!(shards[5][0], 12);
      assert_eq!(shards[5][1], 13); }
    { assert_eq!(shards[6][0], 10);
      assert_eq!(shards[6][1], 11); }
    { assert_eq!(shards[7][0], 14);
      assert_eq!(shards[7][1], 15); }
    { assert_eq!(shards[8][0], 90);
      assert_eq!(shards[8][1], 91); }
    { assert_eq!(shards[9][0], 94);
      assert_eq!(shards[9][1], 95); }

    assert!(r.verify_shards(&shards).unwrap());

    shards[8][0] += 1;
    assert!(!r.verify_shards(&shards).unwrap());
}

#[test]
fn test_verify_too_few_shards() {
    let r = ReedSolomon::new(3, 2).unwrap();

    let shards = make_random_shards!(10, 4);

    assert_eq!(Error::TooFewShards, r.verify_shards(&shards).unwrap_err());
}

#[test]
fn test_verify_shards_with_buffer_incorrect_buffer_sizes() {
    let r = ReedSolomon::new(3, 2).unwrap();

    {
        // Test too few slices in buffer
        let shards = make_random_shards!(100, 5);

        let mut buffer = make_blank_shards(100, 1);

        assert_eq!(Error::TooFewBufferShards,
                   r.verify_shards_with_buffer(&shards,
                                               &mut buffer).unwrap_err());
    }
    {
        // Test too many slices in buffer
        let shards = make_random_shards!(100, 5);

        let mut buffer = make_blank_shards(100, 3);

        assert_eq!(Error::TooManyBufferShards,
                   r.verify_shards_with_buffer(&shards,
                                               &mut buffer).unwrap_err());
    }
    {
        // Test correct number of slices in buffer
        let mut shards = make_random_shards!(100, 5);

        r.encode_shards(&mut shards).unwrap();

        let mut buffer = make_blank_shards(100, 2);

        assert_eq!(true,
                   r.verify_shards_with_buffer(&shards,
                                               &mut buffer).unwrap());
    }
    {
        // Test having first buffer being empty
        let shards = make_random_shards!(100, 5);

        let mut buffer = make_blank_shards(100, 2);
        buffer[0] = vec![].into_boxed_slice();

        assert_eq!(Error::EmptyShard,
                   r.verify_shards_with_buffer(&shards,
                                               &mut buffer).unwrap_err());
    }
    {
        // Test having shards of inconsistent length in buffer
        let shards = make_random_shards!(100, 5);

        let mut buffer = make_blank_shards(100, 2);
        buffer[1] = vec![0; 99].into_boxed_slice();

        assert_eq!(Error::IncorrectShardSize,
                   r.verify_shards_with_buffer(&shards,
                                               &mut buffer).unwrap_err());
    }
}

#[test]
fn test_verify_shards_with_buffer_gives_correct_parity_shards() {
    let r = ReedSolomon::new(10, 3).unwrap();

    for _ in 0..100 {
        let mut shards = make_random_shards!(100, 13);
        let shards_copy = shards.clone();

        r.encode_shards(&mut shards).unwrap();

        {
            let mut buffer = make_random_shards!(100, 3);

            assert!(!r.verify_shards_with_buffer(&shards_copy,
                                                 &mut buffer).unwrap());

            assert_eq_shards(&shards[10..], &buffer);
        }
        {
            let mut buffer = make_random_shards!(100, 3);

            assert!(r.verify_shards_with_buffer(&shards,
                                                &mut buffer).unwrap());

            assert_eq_shards(&shards[10..], &buffer);
        }
    }
}

#[test]
fn test_verify_with_buffer_gives_correct_parity_shards() {
    let r = ReedSolomon::new(10, 3).unwrap();

    for _ in 0..100 {
        let mut slices : [[u8;100]; 13] =
            [[0; 100]; 13];
        for slice in slices.iter_mut() {
            fill_random(slice);
        }
        let slices_copy = slices.clone();

        {
            let mut slice_refs =
                convert_2D_slices!(slices      =>to_mut_vec &mut [u8]);

            r.encode(&mut slice_refs).unwrap();
        }

        {
            let mut buffer : [[u8; 100]; 3] = [[0; 100]; 3];

            {
                let slice_copy_refs =
                    convert_2D_slices!(slices_copy =>to_vec &[u8]);

                for slice in buffer.iter_mut() {
                    fill_random(slice);
                }

                let mut buffer_refs =
                    convert_2D_slices!(buffer =>to_mut_vec &mut [u8]);

                assert!(!r.verify_with_buffer(&slice_copy_refs,
                                              &mut buffer_refs).unwrap());
            }

            for a in 0..3 {
                for b in 0..100 {
                    assert_eq!(slices[10 + a][b], buffer[a][b]);
                }
            }
        }

        {
            let mut buffer : [[u8; 100]; 3] = [[0; 100]; 3];

            {
                let slice_refs =
                    convert_2D_slices!(slices      =>to_vec &[u8]);

                for slice in buffer.iter_mut() {
                    fill_random(slice);
                }

                let mut buffer_refs =
                    convert_2D_slices!(buffer =>to_mut_vec &mut [u8]);

                assert!(r.verify_with_buffer(&slice_refs,
                                             &mut buffer_refs).unwrap());
            }

            for a in 0..3 {
                for b in 0..100 {
                    assert_eq!(slices[10 + a][b], buffer[a][b]);
                }
            }
        }
    }
}

#[test]
fn test_slices_or_shards_count_check() {
    let r = ReedSolomon::new(3, 2).unwrap();

    {
        let mut shards = make_random_shards!(10, 4);

        assert_eq!(Error::TooFewShards, r.encode_shards(&mut shards).unwrap_err());
        assert_eq!(Error::TooFewShards, r.verify_shards(&shards).unwrap_err());

        let mut option_shards = shards_to_option_shards(&shards);

        assert_eq!(Error::TooFewShards, r.reconstruct_shards(&mut option_shards).unwrap_err());
    }
    {
        let mut shards = make_random_shards!(10, 6);

        assert_eq!(Error::TooManyShards, r.encode_shards(&mut shards).unwrap_err());
        assert_eq!(Error::TooManyShards, r.verify_shards(&shards).unwrap_err());

        let mut option_shards = shards_to_option_shards(&shards);

        assert_eq!(Error::TooManyShards, r.reconstruct_shards(&mut option_shards).unwrap_err());
    }
}

#[test]
fn test_check_slices_or_shards_size() {
    let r = ReedSolomon::new(2, 2).unwrap();

    {
        let mut shards = shards!([0, 0, 0],
                                 [0, 1],
                                 [1, 2, 3],
                                 [0, 0, 0]);

        assert_eq!(Error::IncorrectShardSize,
                   r.encode_shards(&mut shards)
                   .unwrap_err());
        assert_eq!(Error::IncorrectShardSize,
                   r.verify_shards(&shards)
                   .unwrap_err());

        let mut option_shards = shards_to_option_shards(&shards);

        assert_eq!(Error::IncorrectShardSize,
                   r.reconstruct_shards(&mut option_shards)
                   .unwrap_err());
    }
    {
        let mut shards = shards!([0, 1],
                                 [0, 1],
                                 [1, 2, 3],
                                 [0, 0, 0]);

        assert_eq!(Error::IncorrectShardSize,
                   r.encode_shards(&mut shards)
                   .unwrap_err());
        assert_eq!(Error::IncorrectShardSize,
                   r.verify_shards(&shards)
                   .unwrap_err());

        let mut option_shards = shards_to_option_shards(&shards);

        assert_eq!(Error::IncorrectShardSize,
                   r.reconstruct_shards(&mut option_shards)
                   .unwrap_err());
    }
    {
        let mut shards = shards!([0, 1],
                                 [0, 1, 4],
                                 [1, 2, 3],
                                 [0, 0, 0]);

        assert_eq!(Error::IncorrectShardSize,
                   r.encode_shards(&mut shards)
                   .unwrap_err());
        assert_eq!(Error::IncorrectShardSize,
                   r.verify_shards(&shards)
                   .unwrap_err());

        let mut option_shards = shards_to_option_shards(&shards);

        assert_eq!(Error::IncorrectShardSize,
                   r.reconstruct_shards(&mut option_shards)
                   .unwrap_err());
    }
    {
        let mut shards = shards!([],
                                 [0, 1, 3],
                                 [1, 2, 3],
                                 [0, 0, 0]);

        assert_eq!(Error::EmptyShard,
                   r.encode_shards(&mut shards)
                   .unwrap_err());
        assert_eq!(Error::EmptyShard,
                   r.verify_shards(&shards)
                   .unwrap_err());

        let mut option_shards = shards_to_option_shards(&shards);

        assert_eq!(Error::EmptyShard,
                   r.reconstruct_shards(&mut option_shards)
                   .unwrap_err());
    }
    {
        let mut option_shards = vec![None, None, None, None];

        assert_eq!(Error::TooFewShardsPresent,
                   r.reconstruct_shards(&mut option_shards)
                   .unwrap_err());
    }
}

#[test]
fn shardbyshard_encode_correctly() {
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(10_000, 13);
        let mut shards_copy = shards.clone();

        r.encode_shards(&mut shards).unwrap();

        for i in 0..10 {
            assert_eq!(i, sbs.cur_input_index());

            sbs.encode_shard(&mut shards_copy).unwrap();
        }

        assert!(sbs.parity_ready());

        assert_eq!(shards, shards_copy);

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut slices : [[u8; 100]; 13] =
            [[0; 100]; 13];
        for slice in slices.iter_mut() {
            fill_random(slice);
        }
        let mut slices_copy = slices.clone();

        {
            let mut slice_refs =
                convert_2D_slices!(slices      =>to_mut_vec &mut [u8]);
            let mut slice_copy_refs =
                convert_2D_slices!(slices_copy =>to_mut_vec &mut [u8]);

            r.encode(&mut slice_refs).unwrap();

            for i in 0..10 {
                assert_eq!(i, sbs.cur_input_index());

                sbs.encode(&mut slice_copy_refs).unwrap();
            }
        }

        assert!(sbs.parity_ready());

        for a in 0..13 {
            for b in 0..100 {
                assert_eq!(slices[a][b], slices_copy[a][b]);
            }
        }

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
}

#[test]
fn shardbyshard_encode_sep_correctly() {
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(10_000, 13);
        let mut shards_copy = shards.clone();

        let (data, parity) =
            shards.split_at_mut(10);
        let (data_copy, parity_copy) =
            shards_copy.split_at_mut(10);

        r.encode_shards_sep(data, parity).unwrap();

        for i in 0..10 {
            assert_eq!(i, sbs.cur_input_index());

            sbs.encode_shard_sep(data_copy, parity_copy).unwrap();
        }

        assert!(sbs.parity_ready());

        assert_eq!(parity, parity_copy);

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut slices : [[u8; 100]; 13] =
            [[0; 100]; 13];
        for slice in slices.iter_mut() {
            fill_random(slice);
        }
        let mut slices_copy = slices.clone();

        {
            let (data, parity) =
                slices.split_at_mut(10);
            let (data_copy, parity_copy) =
                slices_copy.split_at_mut(10);

            let data_refs =
                convert_2D_slices!(data        =>to_mut_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity      =>to_mut_vec &mut [u8]);
            let data_copy_refs =
                convert_2D_slices!(data_copy   =>to_mut_vec &[u8]);
            let mut parity_copy_refs =
                convert_2D_slices!(parity_copy =>to_mut_vec &mut [u8]);

            r.encode_sep(&data_refs, &mut parity_refs).unwrap();

            for i in 0..10 {
                assert_eq!(i, sbs.cur_input_index());

                sbs.encode_sep(&data_copy_refs, &mut parity_copy_refs).unwrap();
            }
        }

        assert!(sbs.parity_ready());

        for a in 0..13 {
            for b in 0..100 {
                assert_eq!(slices[a][b], slices_copy[a][b]);
            }
        }

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
}

#[test]
fn shardbyshard_encode_correctly_more_rigorous() {
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(10_000, 13);
        let mut shards_copy = make_random_shards!(10_000, 13);

        r.encode_shards(&mut shards).unwrap();

        for i in 0..10 {
            assert_eq!(i, sbs.cur_input_index());

            shards_copy[i].clone_from_slice(&shards[i]);
            sbs.encode_shard(&mut shards_copy).unwrap();
            fill_random(&mut shards_copy[i]);
        }

        assert!(sbs.parity_ready());

        for i in 0..10 {
            shards_copy[i].clone_from_slice(&shards[i]);
        }

        assert_eq!(shards, shards_copy);

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut slices : [[u8; 100]; 13] =
            [[0; 100]; 13];
        for slice in slices.iter_mut() {
            fill_random(slice);
        }

        let mut slices_copy : [[u8; 100]; 13] =
            [[0; 100]; 13];
        for slice in slices_copy.iter_mut() {
            fill_random(slice);
        }

        {
            let mut slice_refs =
                convert_2D_slices!(slices      =>to_mut_vec &mut [u8]);
            let mut slice_copy_refs =
                convert_2D_slices!(slices_copy =>to_mut_vec &mut [u8]);

            r.encode(&mut slice_refs).unwrap();

            for i in 0..10 {
                assert_eq!(i, sbs.cur_input_index());

                slice_copy_refs[i].clone_from_slice(&slice_refs[i]);
                sbs.encode(&mut slice_copy_refs).unwrap();
                fill_random(&mut slice_copy_refs[i]);
            }
        }

        for i in 0..10 {
            slices_copy[i].clone_from_slice(&slices[i]);
        }

        assert!(sbs.parity_ready());

        for a in 0..13 {
            for b in 0..100 {
                assert_eq!(slices[a][b], slices_copy[a][b]);
            }
        }

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
}

#[test]
fn shardbyshard_encode_error_handling() {
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(10_000, 13);

        let mut slice_refs =
            convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

        for i in 0..10 {
            assert_eq!(i, sbs.cur_input_index());

            sbs.encode(&mut slice_refs).unwrap();
        }

        assert!(sbs.parity_ready());

        assert_eq!(SBSError::TooManyCalls, sbs.encode(&mut slice_refs).unwrap_err());

        sbs.reset().unwrap();

        for i in 0..1 {
            assert_eq!(i, sbs.cur_input_index());

            sbs.encode(&mut slice_refs).unwrap();
        }

        assert_eq!(SBSError::LeftoverShards, sbs.reset().unwrap_err());

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(100, 13);
        shards[0] = vec![].into_boxed_slice();
        {
            let mut slice_refs =
                convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

            assert_eq!(0, sbs.cur_input_index());

            assert_eq!(SBSError::RSError(Error::EmptyShard),
                       sbs.encode(&mut slice_refs).unwrap_err());

            assert_eq!(0, sbs.cur_input_index());

            assert_eq!(SBSError::RSError(Error::EmptyShard),
                       sbs.encode(&mut slice_refs).unwrap_err());

            assert_eq!(0, sbs.cur_input_index());
        }

        shards[0] = vec![0; 100].into_boxed_slice();

        let mut slice_refs =
            convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

        sbs.encode(&mut slice_refs).unwrap();

        assert_eq!(1, sbs.cur_input_index());
    }
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(100, 13);
        shards[1] = vec![0; 99].into_boxed_slice();
        {
            let mut slice_refs =
                convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

            assert_eq!(0, sbs.cur_input_index());

            assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                       sbs.encode(&mut slice_refs).unwrap_err());

            assert_eq!(0, sbs.cur_input_index());

            assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                       sbs.encode(&mut slice_refs).unwrap_err());

            assert_eq!(0, sbs.cur_input_index());
        }

        shards[1] = vec![0; 100].into_boxed_slice();

        let mut slice_refs =
            convert_2D_slices!(shards =>to_mut_vec &mut [u8]);

        sbs.encode(&mut slice_refs).unwrap();

        assert_eq!(1, sbs.cur_input_index());
    }
}

#[test]
fn shardbyshard_encode_shard_error_handling() {
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(10_000, 13);

        for i in 0..10 {
            assert_eq!(i, sbs.cur_input_index());

            sbs.encode_shard(&mut shards).unwrap();
        }

        assert!(sbs.parity_ready());

        assert_eq!(SBSError::TooManyCalls, sbs.encode_shard(&mut shards).unwrap_err());

        sbs.reset().unwrap();

        for i in 0..1 {
            assert_eq!(i, sbs.cur_input_index());

            sbs.encode_shard(&mut shards).unwrap();
        }

        assert_eq!(SBSError::LeftoverShards, sbs.reset().unwrap_err());

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(100, 13);
        shards[0] = vec![].into_boxed_slice();
        {
            assert_eq!(0, sbs.cur_input_index());

            assert_eq!(SBSError::RSError(Error::EmptyShard),
                       sbs.encode_shard(&mut shards).unwrap_err());

            assert_eq!(0, sbs.cur_input_index());

            assert_eq!(SBSError::RSError(Error::EmptyShard),
                       sbs.encode_shard(&mut shards).unwrap_err());

            assert_eq!(0, sbs.cur_input_index());
        }

        shards[0] = vec![0; 100].into_boxed_slice();

        sbs.encode_shard(&mut shards).unwrap();

        assert_eq!(1, sbs.cur_input_index());
    }
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(100, 13);
        shards[1] = vec![0; 99].into_boxed_slice();
        {
            assert_eq!(0, sbs.cur_input_index());

            assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                       sbs.encode_shard(&mut shards).unwrap_err());

            assert_eq!(0, sbs.cur_input_index());

            assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                       sbs.encode_shard(&mut shards).unwrap_err());

            assert_eq!(0, sbs.cur_input_index());
        }

        shards[1] = vec![0; 100].into_boxed_slice();

        sbs.encode_shard(&mut shards).unwrap();

        assert_eq!(1, sbs.cur_input_index());
    }
}

#[test]
fn shardbyshard_encode_sep_error_handling() {
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(10_000, 13);

        let (data, parity) =
            shards.split_at_mut(10);

        for i in 0..10 {
            assert_eq!(i, sbs.cur_input_index());

            sbs.encode_shard_sep(data, parity).unwrap();
        }

        assert!(sbs.parity_ready());

        assert_eq!(SBSError::TooManyCalls,
                   sbs.encode_shard_sep(data, parity).unwrap_err());

        sbs.reset().unwrap();

        for i in 0..1 {
            assert_eq!(i, sbs.cur_input_index());

            sbs.encode_shard_sep(data, parity).unwrap();
        }

        assert_eq!(SBSError::LeftoverShards, sbs.reset().unwrap_err());

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut slices : [[u8; 100]; 13] =
            [[0; 100]; 13];
        for slice in slices.iter_mut() {
            fill_random(slice);
        }
        {
            let (data, parity) =
                slices.split_at_mut(10);

            let data_refs =
                convert_2D_slices!(data      =>to_mut_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity    =>to_mut_vec &mut [u8]);

            for i in 0..10 {
                assert_eq!(i, sbs.cur_input_index());

                sbs.encode_sep(&data_refs, &mut parity_refs).unwrap();
            }

            assert!(sbs.parity_ready());

            assert_eq!(SBSError::TooManyCalls,
                       sbs.encode_sep(&data_refs, &mut parity_refs).unwrap_err());

            sbs.reset().unwrap();

            for i in 0..1 {
                assert_eq!(i, sbs.cur_input_index());

                sbs.encode_sep(&data_refs, &mut parity_refs).unwrap();
            }
        }

        assert_eq!(SBSError::LeftoverShards, sbs.reset().unwrap_err());

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
    {
        let r = ReedSolomon::new(10, 3).unwrap();

        {
            let mut sbs = ShardByShard::new(&r);

            let mut shards = make_random_shards!(100, 13);
            shards[0] = vec![].into_boxed_slice();

            {
                let (data, parity) =
                    shards.split_at_mut(10);

                let data_refs =
                    convert_2D_slices!(data      =>to_vec &[u8]);
                let mut parity_refs =
                    convert_2D_slices!(parity    =>to_mut_vec &mut [u8]);

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::EmptyShard),
                           sbs.encode_sep(&data_refs, &mut parity_refs).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::EmptyShard),
                           sbs.encode_sep(&data_refs, &mut parity_refs).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());
            }

            shards[0] = vec![0; 100].into_boxed_slice();

            let (data, parity) =
                shards.split_at_mut(10);

            let data_refs =
                convert_2D_slices!(data      =>to_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity    =>to_mut_vec &mut [u8]);

            sbs.encode_sep(&data_refs, &mut parity_refs).unwrap();

            assert_eq!(1, sbs.cur_input_index());
        }
        {
            let mut sbs = ShardByShard::new(&r);

            let mut shards = make_random_shards!(100, 13);
            shards[10] = vec![].into_boxed_slice();
            {
                let (data, parity) =
                    shards.split_at_mut(10);

                let data_refs =
                    convert_2D_slices!(data      =>to_vec &[u8]);
                let mut parity_refs =
                    convert_2D_slices!(parity    =>to_mut_vec &mut [u8]);

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::EmptyShard),
                           sbs.encode_sep(&data_refs, &mut parity_refs).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::EmptyShard),
                           sbs.encode_sep(&data_refs, &mut parity_refs).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());
            }

            shards[10] = vec![0; 100].into_boxed_slice();

            let (data, parity) =
                shards.split_at_mut(10);

            let data_refs =
                convert_2D_slices!(data      =>to_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity    =>to_mut_vec &mut [u8]);

            sbs.encode_sep(&data_refs, &mut parity_refs).unwrap();

            assert_eq!(1, sbs.cur_input_index());
        }
    }
    {
        let r = ReedSolomon::new(10, 3).unwrap();
        {
            let mut sbs = ShardByShard::new(&r);

            let mut shards = make_random_shards!(100, 13);
            shards[1] = vec![0; 99].into_boxed_slice();
            {
                let (data, parity) =
                    shards.split_at_mut(10);

                let data_refs =
                    convert_2D_slices!(data      =>to_vec &[u8]);
                let mut parity_refs =
                    convert_2D_slices!(parity    =>to_mut_vec &mut [u8]);

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                           sbs.encode_sep(&data_refs, &mut parity_refs).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                           sbs.encode_sep(&data_refs, &mut parity_refs).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());
            }

            shards[1] = vec![0; 100].into_boxed_slice();

            let (data, parity) =
                shards.split_at_mut(10);

            let data_refs =
                convert_2D_slices!(data      =>to_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity    =>to_mut_vec &mut [u8]);

            sbs.encode_sep(&data_refs, &mut parity_refs).unwrap();

            assert_eq!(1, sbs.cur_input_index());
        }
        {
            let mut sbs = ShardByShard::new(&r);

            let mut shards = make_random_shards!(100, 13);
            shards[11] = vec![0; 99].into_boxed_slice();
            {
                let (data, parity) =
                    shards.split_at_mut(10);

                let data_refs =
                    convert_2D_slices!(data      =>to_vec &[u8]);
                let mut parity_refs =
                    convert_2D_slices!(parity    =>to_mut_vec &mut [u8]);

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                           sbs.encode_sep(&data_refs, &mut parity_refs).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                           sbs.encode_sep(&data_refs, &mut parity_refs).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());
            }

            shards[11] = vec![0; 100].into_boxed_slice();

            let (data, parity) =
                shards.split_at_mut(10);

            let data_refs =
                convert_2D_slices!(data      =>to_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity    =>to_mut_vec &mut [u8]);

            sbs.encode_sep(&data_refs, &mut parity_refs).unwrap();

            assert_eq!(1, sbs.cur_input_index());
        }
    }
}

#[test]
fn shardbyshard_encode_shard_sep_error_handling() {
    {
        let r       = ReedSolomon::new(10, 3).unwrap();
        let mut sbs = ShardByShard::new(&r);

        let mut shards = make_random_shards!(10_000, 13);

        let (data, parity) =
            shards.split_at_mut(10);

        for i in 0..10 {
            assert_eq!(i, sbs.cur_input_index());

            sbs.encode_shard_sep(data, parity).unwrap();
        }

        assert!(sbs.parity_ready());

        assert_eq!(SBSError::TooManyCalls,
                   sbs.encode_shard_sep(data, parity).unwrap_err());

        sbs.reset().unwrap();

        for i in 0..1 {
            assert_eq!(i, sbs.cur_input_index());

            sbs.encode_shard_sep(data, parity).unwrap();
        }

        assert_eq!(SBSError::LeftoverShards, sbs.reset().unwrap_err());

        sbs.reset_force();

        assert_eq!(0, sbs.cur_input_index());
    }
    {
        let r = ReedSolomon::new(10, 3).unwrap();

        {
            let mut sbs = ShardByShard::new(&r);

            let mut shards = make_random_shards!(100, 13);
            shards[0] = vec![].into_boxed_slice();

            {
                let (data, parity) =
                    shards.split_at_mut(10);

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::EmptyShard),
                           sbs.encode_shard_sep(data, parity).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::EmptyShard),
                           sbs.encode_shard_sep(data, parity).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());
            }

            shards[0] = vec![0; 100].into_boxed_slice();

            let (data, parity) =
                shards.split_at_mut(10);

            sbs.encode_shard_sep(data, parity).unwrap();

            assert_eq!(1, sbs.cur_input_index());
        }
        {
            let mut sbs = ShardByShard::new(&r);

            let mut shards = make_random_shards!(100, 13);
            shards[10] = vec![].into_boxed_slice();
            {
                let (data, parity) =
                    shards.split_at_mut(10);

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::EmptyShard),
                           sbs.encode_shard_sep(data, parity).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::EmptyShard),
                           sbs.encode_shard_sep(data, parity).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());
            }

            shards[10] = vec![0; 100].into_boxed_slice();

            let (data, parity) =
                shards.split_at_mut(10);

            sbs.encode_shard_sep(data, parity).unwrap();

            assert_eq!(1, sbs.cur_input_index());
        }
    }
    {
        let r = ReedSolomon::new(10, 3).unwrap();
        {
            let mut sbs = ShardByShard::new(&r);

            let mut shards = make_random_shards!(100, 13);
            shards[1] = vec![0; 99].into_boxed_slice();
            {
                let (data, parity) =
                    shards.split_at_mut(10);

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                           sbs.encode_shard_sep(data, parity).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                           sbs.encode_shard_sep(data, parity).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());
            }

            shards[1] = vec![0; 100].into_boxed_slice();

            let (data, parity) =
                shards.split_at_mut(10);

            sbs.encode_shard_sep(data, parity).unwrap();

            assert_eq!(1, sbs.cur_input_index());
        }
        {
            let mut sbs = ShardByShard::new(&r);

            let mut shards = make_random_shards!(100, 13);
            shards[11] = vec![0; 99].into_boxed_slice();
            {
                let (data, parity) =
                    shards.split_at_mut(10);

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                           sbs.encode_shard_sep(data, parity).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());

                assert_eq!(SBSError::RSError(Error::IncorrectShardSize),
                           sbs.encode_shard_sep(data, parity).unwrap_err());

                assert_eq!(0, sbs.cur_input_index());
            }

            shards[11] = vec![0; 100].into_boxed_slice();

            let (data, parity) =
                shards.split_at_mut(10);

            sbs.encode_shard_sep(data, parity).unwrap();

            assert_eq!(1, sbs.cur_input_index());
        }
    }
}

#[test]
fn test_encode_single_sep() {
    let r = ReedSolomon::new(10, 3).unwrap();

    {
        let mut shards = make_random_shards!(10, 13);
        let mut shards_copy = shards.clone();

        r.encode_shards(&mut shards).unwrap();

        {
            let (data, parity) =
                shards_copy.split_at_mut(10);

            for i in 0..10 {
                r.encode_single_shard_sep(i, &data[i], parity).unwrap();
            }

        }
        assert!(r.verify_shards(&shards).unwrap());
        assert!(r.verify_shards(&shards_copy).unwrap());

        assert_eq_shards(&shards, &shards_copy);
    }
    {
        let mut slices : [[u8; 100]; 13] =
            [[0; 100]; 13];
        for slice in slices.iter_mut() {
            fill_random(slice);
        }
        let mut slices_copy = slices.clone();

        {
            let mut slice_refs =
                convert_2D_slices!(slices      =>to_mut_vec &mut [u8]);

            let (data_copy, parity_copy) =
                slices_copy.split_at_mut(10);

            let data_copy_refs =
                convert_2D_slices!(data_copy =>to_mut_vec &[u8]);
            let mut parity_copy_refs =
                convert_2D_slices!(parity_copy =>to_mut_vec &mut [u8]);

            r.encode(&mut slice_refs).unwrap();

            for i in 0..10 {
                r.encode_single_sep(i, &data_copy_refs[i], &mut parity_copy_refs).unwrap();
            }
        }

        for a in 0..13 {
            for b in 0..100 {
                assert_eq!(slices[a][b], slices_copy[a][b]);
            }
        }
    }
}

#[test]
fn test_encode_sep() {
    let r = ReedSolomon::new(10, 3).unwrap();

    {
        let mut shards = make_random_shards!(10_000, 13);
        let mut shards_copy = shards.clone();

        r.encode_shards(&mut shards).unwrap();

        {
            let (data, parity) =
                shards_copy.split_at_mut(10);

            r.encode_shards_sep(data, parity).unwrap();
        }

        assert_eq_shards(&shards, &shards_copy);
    }
    {
        let mut slices : [[u8; 100]; 13] =
            [[0; 100]; 13];
        for slice in slices.iter_mut() {
            fill_random(slice);
        }
        let mut slices_copy = slices.clone();

        {
            let (data_copy, parity_copy) =
                slices_copy.split_at_mut(10);

            let mut slice_refs =
                convert_2D_slices!(slices =>to_mut_vec &mut [u8]);
            let data_copy_refs =
                convert_2D_slices!(data_copy =>to_mut_vec &[u8]);
            let mut parity_copy_refs =
                convert_2D_slices!(parity_copy =>to_mut_vec &mut [u8]);

            r.encode(&mut slice_refs).unwrap();

            r.encode_sep(&data_copy_refs, &mut parity_copy_refs).unwrap();
        }

        for a in 0..13 {
            for b in 0..100 {
                assert_eq!(slices[a][b], slices_copy[a][b]);
            }
        }
    }
}

#[test]
fn test_encode_single_sep_error_handling() {
    let r = ReedSolomon::new(10, 3).unwrap();

    {
        let mut shards = make_random_shards!(1000, 13);

        {
            let (data, parity) =
                shards.split_at_mut(10);

            for i in 0..10 {
                r.encode_single_shard_sep(i, &data[i], parity).unwrap();
            }

            assert_eq!(Error::InvalidIndex,
                       r.encode_single_shard_sep(10, &data[0], parity).unwrap_err());
            assert_eq!(Error::InvalidIndex,
                       r.encode_single_shard_sep(11, &data[0], parity).unwrap_err());
            assert_eq!(Error::InvalidIndex,
                       r.encode_single_shard_sep(12, &data[0], parity).unwrap_err());
            assert_eq!(Error::InvalidIndex,
                       r.encode_single_shard_sep(13, &data[0], parity).unwrap_err());
            assert_eq!(Error::InvalidIndex,
                       r.encode_single_shard_sep(14, &data[0], parity).unwrap_err());
        }

        {
            let (data, parity) =
                shards.split_at_mut(11);

            assert_eq!(Error::TooFewParityShards,
                       r.encode_single_shard_sep(0, &data[0], parity).unwrap_err());
        }
        {
            let (data, parity) =
                shards.split_at_mut(9);

            assert_eq!(Error::TooManyParityShards,
                       r.encode_single_shard_sep(0, &data[0], parity).unwrap_err());
        }
    }
    {
        let mut slices : [[u8; 1000]; 13] =
            [[0; 1000]; 13];
        for slice in slices.iter_mut() {
            fill_random(slice);
        }

        {
            let (data, parity) =
                slices.split_at_mut(10);

            let data_refs =
                convert_2D_slices!(data      =>to_mut_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity      =>to_mut_vec &mut [u8]);

            for i in 0..10 {
                r.encode_single_sep(i, &data_refs[i], &mut parity_refs).unwrap();
            }

            assert_eq!(Error::InvalidIndex,
                       r.encode_single_sep(10, &data_refs[0], &mut parity_refs).unwrap_err());
            assert_eq!(Error::InvalidIndex,
                       r.encode_single_sep(11, &data_refs[0], &mut parity_refs).unwrap_err());
            assert_eq!(Error::InvalidIndex,
                       r.encode_single_sep(12, &data_refs[0], &mut parity_refs).unwrap_err());
            assert_eq!(Error::InvalidIndex,
                       r.encode_single_sep(13, &data_refs[0], &mut parity_refs).unwrap_err());
            assert_eq!(Error::InvalidIndex,
                       r.encode_single_sep(14, &data_refs[0], &mut parity_refs).unwrap_err());
        }
        {
            let (data, parity) =
                slices.split_at_mut(11);

            let data_refs =
                convert_2D_slices!(data      =>to_mut_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity      =>to_mut_vec &mut [u8]);

            assert_eq!(Error::TooFewParityShards,
                       r.encode_single_sep(0, &data_refs[0], &mut parity_refs).unwrap_err());
        }
        {
            let (data, parity) =
                slices.split_at_mut(9);

            let data_refs =
                convert_2D_slices!(data      =>to_mut_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity      =>to_mut_vec &mut [u8]);

            assert_eq!(Error::TooManyParityShards,
                       r.encode_single_sep(0, &data_refs[0], &mut parity_refs).unwrap_err());
        }
    }
}

#[test]
fn test_encode_sep_error_handling() {
    let r = ReedSolomon::new(10, 3).unwrap();

    {
        let mut shards = make_random_shards!(1000, 13);

        let (data, parity) =
            shards.split_at_mut(10);

        r.encode_shards_sep(data, parity).unwrap();

        {
            let mut shards = make_random_shards!(1000, 12);
            let (data, parity) =
                shards.split_at_mut(9);

            assert_eq!(Error::TooFewDataShards,
                       r.encode_shards_sep(data, parity).unwrap_err());
        }
        {
            let mut shards = make_random_shards!(1000, 14);
            let (data, parity) =
                shards.split_at_mut(11);

            assert_eq!(Error::TooManyDataShards,
                       r.encode_shards_sep(data, parity).unwrap_err());
        }
        {
            let mut shards = make_random_shards!(1000, 12);
            let (data, parity) =
                shards.split_at_mut(10);

            assert_eq!(Error::TooFewParityShards,
                       r.encode_shards_sep(data, parity).unwrap_err());
        }
        {
            let mut shards = make_random_shards!(1000, 14);
            let (data, parity) =
                shards.split_at_mut(10);

            assert_eq!(Error::TooManyParityShards,
                       r.encode_shards_sep(data, parity).unwrap_err());
        }
    }
    {
        let mut slices : [[u8; 1000]; 13] =
            [[0; 1000]; 13];
        for slice in slices.iter_mut() {
            fill_random(slice);
        }

        let (data, parity) =
            slices.split_at_mut(10);

        let data_refs =
            convert_2D_slices!(data      =>to_mut_vec &[u8]);
        let mut parity_refs =
            convert_2D_slices!(parity      =>to_mut_vec &mut [u8]);

        r.encode_sep(&data_refs, &mut parity_refs).unwrap();

        {
            let mut slices : [[u8; 1000]; 12] =
                [[0; 1000]; 12];
            for slice in slices.iter_mut() {
                fill_random(slice);
            }

            let (data, parity) =
                slices.split_at_mut(9);

            let data_refs =
                convert_2D_slices!(data      =>to_mut_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity      =>to_mut_vec &mut [u8]);

            assert_eq!(Error::TooFewDataShards,
                       r.encode_sep(&data_refs, &mut parity_refs).unwrap_err());
        }
        {
            let mut slices : [[u8; 1000]; 14] =
                [[0; 1000]; 14];
            for slice in slices.iter_mut() {
                fill_random(slice);
            }

            let (data, parity) =
                slices.split_at_mut(11);

            let data_refs =
                convert_2D_slices!(data      =>to_mut_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity      =>to_mut_vec &mut [u8]);

            assert_eq!(Error::TooManyDataShards,
                       r.encode_sep(&data_refs, &mut parity_refs).unwrap_err());
        }
        {
            let mut slices : [[u8; 1000]; 12] =
                [[0; 1000]; 12];
            for slice in slices.iter_mut() {
                fill_random(slice);
            }

            let (data, parity) =
                slices.split_at_mut(10);

            let data_refs =
                convert_2D_slices!(data      =>to_mut_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity      =>to_mut_vec &mut [u8]);

            assert_eq!(Error::TooFewParityShards,
                       r.encode_sep(&data_refs, &mut parity_refs).unwrap_err());
        }
        {
            let mut slices : [[u8; 1000]; 14] =
                [[0; 1000]; 14];
            for slice in slices.iter_mut() {
                fill_random(slice);
            }

            let (data, parity) =
                slices.split_at_mut(10);

            let data_refs =
                convert_2D_slices!(data      =>to_mut_vec &[u8]);
            let mut parity_refs =
                convert_2D_slices!(parity      =>to_mut_vec &mut [u8]);

            assert_eq!(Error::TooManyParityShards,
                       r.encode_sep(&data_refs, &mut parity_refs).unwrap_err());
        }
    }
}

#[test]
fn test_encode_single_error_handling() {
    let r = ReedSolomon::new(10, 3).unwrap();

    {
        let mut shards = make_random_shards!(1000, 13);

        for i in 0..10 {
            r.encode_single_shard(i, &mut shards).unwrap();
        }

        assert_eq!(Error::InvalidIndex,
                   r.encode_single_shard(10, &mut shards).unwrap_err());
        assert_eq!(Error::InvalidIndex,
                   r.encode_single_shard(11, &mut shards).unwrap_err());
        assert_eq!(Error::InvalidIndex,
                   r.encode_single_shard(12, &mut shards).unwrap_err());
        assert_eq!(Error::InvalidIndex,
                   r.encode_single_shard(13, &mut shards).unwrap_err());
        assert_eq!(Error::InvalidIndex,
                   r.encode_single_shard(14, &mut shards).unwrap_err());
    }
    {
        let mut slices : [[u8; 1000]; 13] =
            [[0; 1000]; 13];
        for slice in slices.iter_mut() {
            fill_random(slice);
        }

        let mut slice_refs =
            convert_2D_slices!(slices      =>to_mut_vec &mut [u8]);

        for i in 0..10 {
            r.encode_single(i, &mut slice_refs).unwrap();
        }

        assert_eq!(Error::InvalidIndex,
                   r.encode_single(10, &mut slice_refs).unwrap_err());
        assert_eq!(Error::InvalidIndex,
                   r.encode_single(11, &mut slice_refs).unwrap_err());
        assert_eq!(Error::InvalidIndex,
                   r.encode_single(12, &mut slice_refs).unwrap_err());
        assert_eq!(Error::InvalidIndex,
                   r.encode_single(13, &mut slice_refs).unwrap_err());
        assert_eq!(Error::InvalidIndex,
                   r.encode_single(14, &mut slice_refs).unwrap_err());
    }
}
