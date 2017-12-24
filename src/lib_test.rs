#![cfg(test)]
#![allow(dead_code)]
extern crate rand;

use super::*;
use super::shard_utils;
use self::rand::{thread_rng, Rng};
//use std::rc::Rc;

macro_rules! make_random_shards {
    ($per_shard:expr, $size:expr) => {{
        let mut shards = Vec::with_capacity(13);
        for _ in 0..$size {
            shards.push(make_blank_shard($per_shard));
        }

        for s in shards.iter_mut() {
            fill_random(s);
        }

        shards
    }}
}

fn assert_eq_shards(s1 : &Vec<Shard>, s2 : &Vec<Shard>) {
    assert_eq!(s1.len(), s2.len());
    for i in 0..s1.len() {
        assert_eq!(s1[i],
                   s2[i]);
    }
}

fn is_increasing_and_contains_data_row(indices : &Vec<usize>) -> bool {
    let cols = indices.len();
    for i in 0..cols-1 {
        if indices[i] >= indices[i+1] {
            return false
        }
    }
    return indices[0] < cols
}

fn increment_indices(indices : &mut Vec<usize>,
                     index_bound : usize) -> bool {
    for i in (0..indices.len()).rev() {
        indices[i] += 1;
        if indices[i] < index_bound {
            break;
        }

        if i == 0 {
            return false
        }

        indices[i] = 0
    }

    return true
}

fn increment_indices_until_increasing_and_contains_data_row(indices : &mut Vec<usize>, max_index : usize) -> bool {
    loop {
        let valid = increment_indices(indices, max_index);
        if !valid {
            return false
        }

        if is_increasing_and_contains_data_row(indices) {
            return true
        }
    }
}

fn find_singular_sub_matrix(m : Matrix) -> Option<Matrix> {
    let rows = m.row_count();
    let cols = m.col_count();
    let mut row_indices = Vec::with_capacity(cols);
    while increment_indices_until_increasing_and_contains_data_row(&mut row_indices, rows) {
        let mut sub_matrix = Matrix::new(cols, cols);
        for i in 0..row_indices.len() {
            let r = row_indices[i];
            for c in 0..cols {
                sub_matrix.set(i, c, m.get(r, c));
            }
        }

        match sub_matrix.invert() {
            Err(matrix::Error::SingularMatrix) => return Some(sub_matrix),
            whatever => whatever.unwrap()
        };
    }
    None
}

fn fill_random(arr : &mut Shard) {
    for a in arr.iter_mut() {
        *a = rand::random::<u8>();
    }
}

fn assert_eq_shards_with_range(shards1    : &Vec<Shard>,
                               shards2    : &Vec<Shard>,
                               offset     : usize,
                               byte_count : usize) {
    for s in 0..shards1.len() {
        let slice1 = &shards1[s][offset..offset + byte_count];
        let slice2 = &shards2[s][offset..offset + byte_count];
        assert_eq!(slice1, slice2);
    }
}

#[test]
#[should_panic]
fn test_no_data_shards() {
    ReedSolomon::new(0, 1); }

#[test]
#[should_panic]
fn test_no_parity_shards() {
    ReedSolomon::new(1, 0); }

#[test]
fn test_shard_count() {
    let mut rng = thread_rng();
    for _ in 0..10 {
        let data_shard_count   = rng.gen_range(1, 128);
        let parity_shard_count = rng.gen_range(1, 128);

        let total_shard_count = data_shard_count + parity_shard_count;

        let r = ReedSolomon::new(data_shard_count, parity_shard_count);

        assert_eq!(data_shard_count,   r.data_shard_count());
        assert_eq!(parity_shard_count, r.parity_shard_count());
        assert_eq!(total_shard_count,  r.total_shard_count());
    }
}

#[test]
#[should_panic]
fn test_calc_byte_count_byte_count_is_zero_case1() {
    let shards = make_random_shards!(1_000, 1);

    shard_utils::helper::calc_byte_count(&shards,
                                         Some(0)); }

#[test]
#[should_panic]
fn test_calc_byte_count_byte_count_is_zero_case2() {
    let shards = make_random_shards!(1_000, 0);

    shard_utils::helper::calc_byte_count(&shards,
                                         None); }

#[test]
#[should_panic]
fn test_calc_byte_count_option_shards_byte_count_is_zero_case1() {
    let shards = make_random_shards!(1_000, 1);
    let option_shards = shards_into_option_shards(shards);

    shard_utils::helper::calc_byte_count_option_shards(&option_shards,
                                                       Some(0)); }

#[test]
#[should_panic]
fn test_calc_byte_count_option_shards_byte_count_is_zero_case2() {
    let shards = make_random_shards!(1_000, 0);
    let option_shards = shards_into_option_shards(shards);

    shard_utils::helper::calc_byte_count_option_shards(&option_shards,
                                                       None); }

#[test]
#[should_panic]
fn test_calc_byte_count_option_shards_no_shards_present() {
    let shards = make_random_shards!(1_000, 2);

    let mut option_shards = shards_into_option_shards(shards);

    option_shards[0] = None;
    option_shards[1] = None;

    shard_utils::helper::calc_byte_count_option_shards(&option_shards,
                                                       None); }

/*
#[test]
fn test_shards_into_option_shards_into_shards() {
    for _ in 0..100 {
        let shards = make_random_shards!(1_000, 10);
        let expect = shards.clone();
        let inter  = shards_into_option_shards(shards);
        let result = option_shards_into_shards(inter);

        assert_eq_shards(&expect, &result);
    }
}

#[test]
fn test_shards_to_option_shards_to_shards() {
    for _ in 0..100 {
        let shards = make_random_shards!(1_000, 10);
        let expect = shards.clone();
        let option_shards =
            shards_to_option_shards(&shards);
        let result        =
            option_shards_to_shards(&option_shards, None, None);

        assert_eq_shards(&expect, &result);
    }
}

#[test]
#[should_panic]
fn test_option_shards_to_shards_missing_shards_case1() {
    let shards = make_random_shards!(1_000, 10);
    let mut option_shards = shards_into_option_shards(shards);

    option_shards[0] = None;

    option_shards_to_shards(&option_shards, None, None);
}

#[test]
fn test_option_shards_to_shards_missing_shards_case2() {
    let shards = make_random_shards!(1_000, 10);
    let mut option_shards = shards_into_option_shards(shards);

    option_shards[0] = None;
    option_shards[9] = None;

    option_shards_to_shards(&option_shards, Some(1), Some(8));
}

#[test]
#[should_panic]
fn test_option_shards_into_missing_shards() {
    let shards = make_random_shards!(1_000, 10);
    let mut option_shards = shards_into_option_shards(shards);

    option_shards[2] = None;

    option_shards_into_shards(option_shards);
}

#[test]
#[should_panic]
fn test_option_shards_to_shards_too_few_shards() {
    let shards = make_random_shards!(1_000, 10);
    let option_shards = shards_into_option_shards(shards);

    option_shards_to_shards(&option_shards,
                            None,
                            Some(11));
}

#[test]
fn test_reedsolomon_clone() {
    let r1 = ReedSolomon::new(10, 3);
    let r2 = r1.clone();

    assert_eq!(r1, r2);
}

#[test]
#[should_panic]
fn test_reedsolomon_too_many_shards() {
    ReedSolomon::new(256, 1); }

#[test]
#[should_panic]
fn test_check_buffer_and_sizes_total_shard_count() {
    let r = ReedSolomon::new(10, 3);
    let shards = make_random_shards!(1_000, 12);

    r.check_buffer_and_sizes(&shards, 0, 12);
}

#[test]
#[should_panic]
fn test_check_buffer_and_sizes_shards_same_size() {
    let r = ReedSolomon::new(3, 2);
    let shards = shards!([0, 1, 2],
                         [0, 1, 2, 4],
                         [0, 1, 2],
                         [0, 1, 2],
                         [0, 1, 2]);

    r.check_buffer_and_sizes(&shards, 0, 3);
}

#[test]
#[should_panic]
fn test_check_buffer_and_sizes_shards_too_small() {
    let r = ReedSolomon::new(3, 2);
    let shards = shards!([0, 1, 2],
                         [0, 1, 2],
                         [0, 1, 2],
                         [0, 1, 2],
                         [0, 1, 2]);

    r.check_buffer_and_sizes(&shards, 0, 4);
}

#[test]
#[should_panic]
fn test_check_buffer_and_sizes_option_shards_total_shard_count() {
    let r = ReedSolomon::new(10, 3);
    let shards =
        shards_into_option_shards(
            make_random_shards!(1_000, 12));

    r.check_buffer_and_sizes_option_shards(&shards, 0, 12);
}

#[test]
#[should_panic]
fn test_check_buffer_and_sizes_option_shards_shards_same_size() {
    let r = ReedSolomon::new(3, 2);
    let shards =
        shards_into_option_shards(
            shards!([0, 1, 2],
                    [0, 1, 2, 4],
                    [0, 1, 2],
                    [0, 1, 2],
                    [0, 1, 2]));

    r.check_buffer_and_sizes_option_shards(&shards, 0, 3);
}

#[test]
#[should_panic]
fn test_check_buffer_and_sizes_option_shards_shards_too_small() {
    let r = ReedSolomon::new(3, 2);
    let shards =
        shards_into_option_shards(
            shards!([0, 1, 2],
                    [0, 1, 2],
                    [0, 1, 2],
                    [0, 1, 2],
                    [0, 1, 2]));

    r.check_buffer_and_sizes_option_shards(&shards, 0, 4);
}

#[test]
fn test_shallow_clone_shards() {
    let shards1 = make_random_shards!(1_000, 10);

    for v in shards1.iter() {
        assert_eq!(1, Arc::strong_count(v));
    }

    let shards2 = shards1.clone();

    for v in shards1.iter() {
        assert_eq!(2, Arc::strong_count(v));
    }
    for v in shards2.iter() {
        assert_eq!(2, Arc::strong_count(v));
    }
}

#[test]
fn test_deep_clone_shards() {
    let shards1 = make_random_shards!(1_000, 10);

    for v in shards1.iter() {
        assert_eq!(1, Arc::strong_count(v));
    }

    let shards2 = deep_clone_shards(&shards1);

    for v in shards1.iter() {
        assert_eq!(1, Arc::strong_count(v));
    }
    for v in shards2.iter() {
        assert_eq!(1, Arc::strong_count(v));
    }
}

#[test]
fn test_shallow_clone_option_shards() {
    let shards1 =
        shards_into_option_shards(
            make_random_shards!(1_000, 10));

    for v in shards1.iter() {
        if let Some(ref x) = *v {
            assert_eq!(1, Arc::strong_count(x));
        }
    }

    let shards2 = shards1.clone();

    for v in shards1.iter() {
        if let Some(ref x) = *v {
            assert_eq!(2, Arc::strong_count(x));
        }
    }
    for v in shards2.iter() {
        if let Some(ref x) = *v {
            assert_eq!(2, Arc::strong_count(x));
        }
    }
}

#[test]
fn test_deep_clone_option_shards() {
    let mut shards1 =
        shards_into_option_shards(
            make_random_shards!(1_000, 10));

    for v in shards1.iter() {
        if let Some(ref x) = *v {
            assert_eq!(1, Arc::strong_count(x));
        }
    }

    let shards2 = deep_clone_option_shards(&shards1);

    for v in shards1.iter() {
        if let Some(ref x) = *v {
            assert_eq!(1, Arc::strong_count(x));
        }
    }
    for v in shards2.iter() {
        if let Some(ref x) = *v {
            assert_eq!(1, Arc::strong_count(x));
        }
    }

    shards1[0] = None;
    shards1[4] = None;
    shards1[7] = None;

    let shards3 = deep_clone_option_shards(&shards1);

    if let Some(_) = shards3[0] { panic!() }
    if let Some(_) = shards3[4] { panic!() }
    if let Some(_) = shards3[7] { panic!() }
}

#[test]
fn test_rc_counts_carries_over_decode_missing() {
    let r = ReedSolomon::new(3, 2);

    let mut master_copy = shards!([0, 1,  2,  3],
                                  [4, 5,  6,  7],
                                  [8, 9, 10, 11],
                                  [0, 0,  0,  0],
                                  [0, 0,  0,  0]);

    r.encode_parity(&mut master_copy, None, None);

    // the cloning below increases rc counts from 1 to 2
    let mut shards = shards_into_option_shards(master_copy.clone());

    shards[0] = None;
    shards[4] = None;

    // the new shards constructed by decode_missing
    // will have rc count of just 1
    r.decode_missing(&mut shards, None, None).unwrap();
    
    let result = option_shards_into_shards(shards);
    
    assert!(r.is_parity_correct(&result, None, None));
    assert_eq!(1, Arc::strong_count(&result[0]));
    assert_eq!(2, Arc::strong_count(&result[1]));
    assert_eq!(2, Arc::strong_count(&result[2]));
    assert_eq!(2, Arc::strong_count(&result[3]));
    assert_eq!(1, Arc::strong_count(&result[4]));
}

#[test]
fn test_shards_to_option_shards_does_not_change_rc_counts() {
    let shards = make_random_shards!(1_000, 10);

    let option_shards =
        shards_to_option_shards(&shards);

    for v in shards.iter() {
        assert_eq!(1, Arc::strong_count(v));
    }
    for v in option_shards.iter() {
        if let Some(ref x) = *v {
            assert_eq!(1, Arc::strong_count(x));
        }
    }
}

#[test]
fn test_shards_into_option_shards_does_not_change_rc_counts() {
    let shards = make_random_shards!(1_000, 10);

    let option_shards =
        shards_into_option_shards(shards);

    for v in option_shards.iter() {
        if let Some(ref x) = *v {
            assert_eq!(1, Arc::strong_count(x));
        }
    }
}

#[test]
fn test_option_shards_to_shards_does_not_change_rc_counts() {
    let option_shards =
        shards_to_option_shards(
            &make_random_shards!(1_000, 10));

    let shards =
        option_shards_to_shards(&option_shards, None, None);

    for v in option_shards.iter() {
        if let Some(ref x) = *v {
            assert_eq!(1, Arc::strong_count(x));
        }
    }
    for v in shards.iter() {
        assert_eq!(1, Arc::strong_count(v));
    }
}

#[test]
fn test_option_shards_into_shards_does_not_change_rc_counts() {
    let option_shards =
        shards_to_option_shards(
            &make_random_shards!(1_000, 10));

    let shards =
        option_shards_into_shards(option_shards);

    for v in shards.iter() {
        assert_eq!(1, Arc::strong_count(v));
    }
}

#[test]
fn test_encoding() {
    let per_shard = 50_000;

    let r = ReedSolomon::new(10, 3);

    let mut shards = make_random_shards!(per_shard, 13);

    r.encode_parity(&mut shards, None, None);
    assert!(r.is_parity_correct(&shards, None, None));
}

#[test]
fn test_encoding_with_range() {
    let per_shard = 50_000;

    let r = ReedSolomon::new(10, 3);

    let mut shards = make_random_shards!(per_shard, 13);

    r.encode_parity(&mut shards, Some(7), Some(100));
    assert!(r.is_parity_correct(&shards, Some(7), Some(100)));
}

#[test]
fn test_decode_missing() {
    let per_shard = 100_000;

    let r = ReedSolomon::new(8, 5);

    let mut shards = make_random_shards!(per_shard, 13);

    r.encode_parity(&mut shards, None, None);

    let master_copy = shards.clone();

    let mut shards = shards_to_option_shards(&shards);

    // Try to decode with all shards present
    r.decode_missing(&mut shards,
                     None, None).unwrap();
    {
        let shards = option_shards_to_shards(&shards, None, None);
        assert!(r.is_parity_correct(&shards, None, None));
        assert_eq_shards(&shards, &master_copy);
    }

    // Try to decode with 10 shards
    shards[0] = None;
    shards[2] = None;
    //shards[4] = None;
    r.decode_missing(&mut shards,
                     None, None).unwrap();
    {
        let shards = option_shards_to_shards(&shards, None, None);
        assert!(r.is_parity_correct(&shards, None, None));
        assert_eq_shards(&shards, &master_copy);
    }

    // Try to deocde with 6 data and 4 parity shards
    shards[0] = None;
    shards[2] = None;
    shards[12] = None;
    r.decode_missing(&mut shards,
                     None, None).unwrap();
    {
        let shards = option_shards_to_shards(&shards, None, None);
        assert!(r.is_parity_correct(&shards, None, None));
    }

    // Try to decode with 7 data and 1 parity shards
    shards[0] = None;
    shards[1] = None;
    shards[9] = None;
    shards[10] = None;
    shards[11] = None;
    shards[12] = None;
    assert_eq!(r.decode_missing(&mut shards,
                                None, None).unwrap_err(),
               Error::NotEnoughShards);
}

#[test]
fn test_decode_missing_with_range() {
    let per_shard = 100_000;

    let offset = 7;
    let byte_count = 100;
    let op_offset = Some(offset);
    let op_byte_count = Some(byte_count);

    let r = ReedSolomon::new(8, 5);

    let mut shards = make_random_shards!(per_shard, 13);

    r.encode_parity(&mut shards, Some(7), Some(100));

    let master_copy = shards.clone();

    let mut shards = shards_to_option_shards(&shards);

    // Try to decode with all shards present
    r.decode_missing(&mut shards,
                     Some(7), Some(100)).unwrap();
    {
        let shards = option_shards_to_shards(&shards, None, None);
        assert!(r.is_parity_correct(&shards, op_offset, op_byte_count));
        assert_eq_shards_with_range(&shards, &master_copy, offset, byte_count);
    }

    // Try to decode with 10 shards
    shards[0] = None;
    shards[2] = None;
    //shards[4] = None;
    r.decode_missing(&mut shards,
                     op_offset, op_byte_count).unwrap();
    {
        let shards = option_shards_to_shards(&shards, None, None);
        assert!(r.is_parity_correct(&shards, op_offset, op_byte_count));
        assert_eq_shards_with_range(&shards, &master_copy, offset, byte_count);
    }

    // Try to deocde with 6 data and 4 parity shards
    shards[0] = None;
    shards[2] = None;
    shards[12] = None;
    r.decode_missing(&mut shards,
                     None, None).unwrap();
    {
        let shards = option_shards_to_shards(&shards, None, None);
        assert!(r.is_parity_correct(&shards, op_offset, op_byte_count));
        assert_eq_shards_with_range(&shards, &master_copy, offset, byte_count);
    }

    // Try to decode with 7 data and 1 parity shards
    shards[0] = None;
    shards[1] = None;
    shards[9] = None;
    shards[10] = None;
    shards[11] = None;
    shards[12] = None;
    assert_eq!(r.decode_missing(&mut shards,
                                op_offset, op_byte_count).unwrap_err(),
               Error::NotEnoughShards);
}

#[test]
fn test_is_parity_correct() {
    let per_shard = 33_333;

    let r = ReedSolomon::new(10, 4);

    let mut shards = make_random_shards!(per_shard, 14);

    r.encode_parity(&mut shards, None, None);
    assert!(r.is_parity_correct(&shards, None, None));

    // corrupt shards
    fill_random(&mut shards[5]);
    assert!(!r.is_parity_correct(&shards, None, None));

    // Re-encode
    r.encode_parity(&mut shards, None, None);
    fill_random(&mut shards[1]);
    assert!(!r.is_parity_correct(&shards, None, None));
}

#[test]
fn test_is_parity_correct_with_range() {
    let per_shard = 33_333;

    let offset = 7;
    let byte_count = 100;
    let op_offset = Some(offset);
    let op_byte_count = Some(byte_count);

    let r = ReedSolomon::new(10, 4);

    let mut shards = make_random_shards!(per_shard, 14);

    r.encode_parity(&mut shards, op_offset, op_byte_count);
    assert!(r.is_parity_correct(&shards, op_offset, op_byte_count));

    // corrupt shards
    fill_random(&mut shards[5]);
    assert!(!r.is_parity_correct(&shards, op_offset, op_byte_count));

    // Re-encode
    r.encode_parity(&mut shards, op_offset, op_byte_count);
    fill_random(&mut shards[1]);
    assert!(!r.is_parity_correct(&shards, op_offset, op_byte_count));
}

#[test]
fn test_one_encode() {
    let r = ReedSolomon::new(5, 5);

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

    r.encode_parity(&mut shards, None, None);
    { assert_eq!(shards[5].read().unwrap()[0], 12);
      assert_eq!(shards[5].read().unwrap()[1], 13); }
    { assert_eq!(shards[6].read().unwrap()[0], 10);
      assert_eq!(shards[6].read().unwrap()[1], 11); }
    { assert_eq!(shards[7].read().unwrap()[0], 14);
      assert_eq!(shards[7].read().unwrap()[1], 15); }
    { assert_eq!(shards[8].read().unwrap()[0], 90);
      assert_eq!(shards[8].read().unwrap()[1], 91); }
    { assert_eq!(shards[9].read().unwrap()[0], 94);
      assert_eq!(shards[9].read().unwrap()[1], 95); }

    assert!(r.is_parity_correct(&shards, None, None));

    shards[8].write().unwrap()[0] += 1;
    assert!(!r.is_parity_correct(&shards, None, None));
}
*/
