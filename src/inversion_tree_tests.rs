#![cfg(test)]
extern crate rand;

use std::sync::Arc;

use super::matrix::Matrix;
use super::inversion_tree::*;
use super::matrix_tests::make_random_matrix;

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

quickcheck! {
    fn qc_tree(data_shards   : usize,
               parity_shards : usize,
               matrix_count  : usize) -> bool {
        if data_shards   == 0 { return true; }
        if parity_shards == 0 { return true; }
        if data_shards + parity_shards > 256 { return true; }

        let tree = InversionTree::new(data_shards, parity_shards);

        let mut matrices : Vec<(Vec<usize>, Matrix)> =
            Vec::with_capacity(matrix_count);
        for _ in 0..matrix_count {
            let invalid_indices = make_random_invalid_indices(data_shards,
                                                              parity_shards);
            let matrix = make_random_matrix(data_shards);
            match tree.insert_inverted_matrix(&invalid_indices,
                                              &Arc::new(matrix.clone())) {
                Ok(())                 => {},
                Err(Error::AlreadySet) => {},
                Err(Error::NotSquare)  => panic!(),
            }
            matrices.push((invalid_indices,
                           matrix));
        }

        false
    }
}
