#![cfg(test)]
use rand;

use std::sync::Arc;
use std::collections::HashMap;

use matrix::Matrix;
use inversion_tree::*;
use matrix_tests::make_random_matrix;

use quickcheck::{QuickCheck,
                 Gen,
                 Arbitrary};

macro_rules! matrix {
    (
        $(
            [ $( $x:expr ),+ ]
        ),*
    ) => (
        Matrix::new_with_data(vec![ $( vec![$( $x ),*] ),* ])
    );
    ($rows:expr, $cols:expr) => (Matrix::new($rows, $cols));
}

#[test]
fn test_new_inversion_tree() {
    let tree = InversionTree::new(3, 2);

    let children = tree.root.lock().unwrap().children.len();
    assert_eq!(5, children);

    let expect = matrix!([1, 0, 0],
                         [0, 1, 0],
                         [0, 0, 1]);
    assert_eq!(expect, *tree.get_inverted_matrix(&[]).unwrap());
}

#[test]
fn test_get_inverted_matrix() {
    let tree = InversionTree::new(3, 2);

    let matrix = &*tree.get_inverted_matrix(&[]).unwrap();

    let expect = matrix!([1, 0, 0],
                         [0, 1, 0],
                         [0, 0, 1]);

    assert_eq!(expect, *matrix);

    let matrix = tree.get_inverted_matrix(&[1]);
    assert_eq!(None, matrix);

    let matrix = tree.get_inverted_matrix(&[1, 2]);
    assert_eq!(None, matrix);

    let matrix = Matrix::new(3, 3);
    let matrix_copy = matrix.clone();
    tree.insert_inverted_matrix(&[1], &Arc::new(matrix)).unwrap();

    let cached_matrix = tree.get_inverted_matrix(&[1]).unwrap();
    assert_eq!(matrix_copy, *cached_matrix);
}

#[test]
fn test_insert_inverted_matrix() {
    let tree = InversionTree::new(3, 2);

    let matrix = Matrix::new(3, 3);
    let matrix_copy = matrix.clone();

    tree.insert_inverted_matrix(&[1], &Arc::new(matrix)).unwrap();
    tree.insert_inverted_matrix(&[], &Arc::new(matrix_copy)).unwrap_err();

    let matrix = Matrix::new(3, 2);
    tree.insert_inverted_matrix(&[2], &Arc::new(matrix)).unwrap_err();

    let matrix = Matrix::new(3, 3);
    tree.insert_inverted_matrix(&[0, 1], &Arc::new(matrix)).unwrap();
}

#[test]
fn test_double_insert_inverted_matrix() {
    let tree = InversionTree::new(3, 2);

    let matrix = Matrix::new(3, 3);
    let matrix_copy1 = matrix.clone();
    let matrix_copy2 = matrix.clone();

    tree.insert_inverted_matrix(&[1], &Arc::new(matrix)).unwrap();
    tree.insert_inverted_matrix(&[1], &Arc::new(matrix_copy1)).unwrap();

    let cached_matrix = tree.get_inverted_matrix(&[1]).unwrap();
    assert_eq!(matrix_copy2, *cached_matrix);
}

#[test]
fn test_extended_inverted_matrix() {
    let tree = InversionTree::new(10, 3);
    let matrix = Matrix::new(10, 10);
    let matrix_copy = matrix.clone();
    let matrix2 = matrix!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [1, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
    let matrix2_copy = matrix2.clone();
    let matrix3 = matrix!([9, 1, 2, 3, 4, 5, 6, 7, 8, 0],
                          [9, 1, 2, 3, 4, 5, 6, 7, 8, 0],
                          [9, 1, 2, 3, 4, 5, 6, 7, 8, 0],
                          [9, 1, 2, 3, 4, 5, 6, 7, 8, 0],
                          [9, 1, 2, 3, 4, 5, 6, 7, 8, 0],
                          [1, 1, 2, 3, 4, 5, 6, 7, 8, 0],
                          [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                          [1, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
    let matrix3_copy = matrix3.clone();

    tree.insert_inverted_matrix(&[1, 2], &Arc::new(matrix)).unwrap();

    let result = tree.get_inverted_matrix(&[1, 2]).unwrap();
    assert_eq!(matrix_copy, *result);

    tree.insert_inverted_matrix(&[1, 2, 5, 12], &Arc::new(matrix2)).unwrap();
    let result = tree.get_inverted_matrix(&[1, 2, 5, 12]).unwrap();
    assert_eq!(matrix2_copy, *result);

    tree.insert_inverted_matrix(&[0, 3, 4, 11], &Arc::new(matrix3)).unwrap();
    let result = tree.get_inverted_matrix(&[0, 3, 4, 11]).unwrap();
    assert_eq!(matrix3_copy, *result);
}

fn make_random_invalid_indices(data_shards   : usize,
                               parity_shards : usize) -> Vec<usize> {
    let mut invalid_count = 0;
    let mut res = Vec::new();
    for i in 0..data_shards + parity_shards {
        if rand::random::<bool>() && invalid_count < parity_shards {
            res.push(i);
            invalid_count += 1;
        }
    }
    res
}

#[derive(Debug, Clone)]
struct QCTreeTestParam {
    data_shards   : usize,
    parity_shards : usize,
    matrix_count  : usize,
    iter_order    : Vec<usize>,
    read_count    : usize,
}

impl Arbitrary for QCTreeTestParam {
    fn arbitrary<G : Gen>(g : &mut G) -> Self {
        let size   = g.size();

        let matrix_count = 5 + size % 100;

        let mut iter_order = Vec::with_capacity(matrix_count);
        for _ in 0..matrix_count {
            iter_order.push(rand::random::<usize>());
        }

        QCTreeTestParam {
            data_shards   : 1 + size % 50,
            parity_shards : 1 + size % 50,
            matrix_count,
            iter_order,
            read_count    : 2 + size % 10,
        }
    }
}

#[test]
fn qc_tree_same_as_hash_map() {
    QuickCheck::new()
        .min_tests_passed(10_000)
        .tests(11_000)
        .max_tests(100_000)
        .quickcheck(
            qc_tree_same_as_hash_map_prop as fn(QCTreeTestParam) -> bool);
}

// inversion tree is functionally the same as a map
// but more efficient
fn qc_tree_same_as_hash_map_prop(param : QCTreeTestParam)
                                 -> bool {
    let tree = InversionTree::new(param.data_shards,
                                  param.parity_shards);
    let mut map = HashMap::with_capacity(param.matrix_count);

    let mut invalid_indices_set = Vec::with_capacity(param.matrix_count);

    for _ in 0..param.matrix_count {
        let invalid_indices = make_random_invalid_indices(param.data_shards,
                                                          param.parity_shards);
        let matrix = make_random_matrix(param.data_shards);
        match tree.insert_inverted_matrix(&invalid_indices,
                                          &Arc::new(matrix.clone())) {
            Ok(())                 => {
                map.insert(invalid_indices.clone(), matrix);
                invalid_indices_set.push(invalid_indices);
            },
            Err(Error::AlreadySet) => {},
            Err(Error::NotSquare)  => panic!(),
        }
    }

    for _ in 0..param.read_count {
        // iterate according to the provided order
        if invalid_indices_set.len() > 0 {
            for i in param.iter_order.iter() {
                let i = i % invalid_indices_set.len();

                let invalid_indices = &invalid_indices_set[i];

                let matrix_in_tree =
                    tree.get_inverted_matrix(invalid_indices).unwrap();
                let matrix_in_map =
                    map.get(invalid_indices).unwrap();
                if matrix_in_tree.as_ref() != matrix_in_map {
                    return false;
                }
            }
        }

        // iterate through the insertion order
        for ref invalid_indices in invalid_indices_set.iter() {
            let matrix_in_tree =
                tree.get_inverted_matrix(invalid_indices).unwrap();
            let matrix_in_map =
                map.get(*invalid_indices).unwrap();
            if matrix_in_tree.as_ref() != matrix_in_map {
                return false;
            }
        }

        // iterate through the map's order
        for (ref invalid_indices,
             ref matrix_in_map) in map.iter() {
            let matrix_in_tree =
                tree.get_inverted_matrix(invalid_indices).unwrap();
            if matrix_in_tree.as_ref() != *matrix_in_map {
                return false;
            }
        }
    }

    true
}
