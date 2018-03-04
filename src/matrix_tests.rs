#![cfg(test)]
use super::Matrix;

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

use super::quickcheck::{Arbitrary, Gen};
use super::misc_utils::fill_random;

impl Arbitrary for Matrix {
    fn arbitrary<G : Gen>(g : &mut G) -> Self {
        let size = g.size();

        let mut vec : Vec<Vec<u8>> = vec![vec![0; size]; size];
        for v in vec.iter_mut() {
            fill_random(v);
        }

        Matrix::new_with_data(vec)
    }
}

#[test]
fn test_matrix_col_count() {
    let m1 = matrix!([1, 0, 0]);
    let m2 = matrix!([0, 0, 0],
                     [0, 0, 0]);
    let m3 = Matrix::new(1, 4);

    assert_eq!(3, m1.col_count());
    assert_eq!(3, m2.col_count());
    assert_eq!(4, m3.col_count());
}

#[test]
fn test_matrix_row_count() {
    let m1 = matrix!([1, 0, 0]);
    let m2 = matrix!([0, 0, 0],
                     [0, 0, 0]);
    let m3 = Matrix::new(1, 4);

    assert_eq!(1, m1.row_count());
    assert_eq!(2, m2.row_count());
    assert_eq!(1, m3.row_count());
}

#[test]
fn test_matrix_swap_rows() {
    {
        let mut m1 = matrix!([1, 2, 3],
                             [4, 5, 6],
                             [7, 8, 9]);
        let expect = matrix!([7, 8, 9],
                             [4, 5, 6],
                             [1, 2, 3]);
        m1.swap_rows(0, 2);
        assert_eq!(expect, m1);
    }
    {
        let mut m1 = matrix!([1, 2, 3],
                             [4, 5, 6],
                             [7, 8, 9]);
        let expect = m1.clone();
        m1.swap_rows(0, 0);
        assert_eq!(expect, m1);
        m1.swap_rows(1, 1);
        assert_eq!(expect, m1);
        m1.swap_rows(2, 2);
        assert_eq!(expect, m1);
    }
}

#[test]
#[should_panic]
fn test_inconsistent_row_sizes() {
    matrix!([1, 0, 0], [0, 1], [0, 0, 1]); }

#[test]
#[should_panic]
fn test_incompatible_multiply() {
    let m1 = matrix!([0, 1],
                     [0, 1],
                     [0, 1]);
    let m2 = matrix!([0, 1, 2]);

    m1.multiply(&m2);
}

#[test]
#[should_panic]
fn test_incompatible_augment() {
    let m1 = matrix!([0, 1]);
    let m2 = matrix!([0, 1],
                     [2, 3]);

    m1.augment(&m2);
}

#[test]
fn test_matrix_identity() {
    let m1 = Matrix::identity(3);
    let m2 = matrix!([1, 0, 0],
                     [0, 1, 0],
                     [0, 0, 1]);
    assert_eq!(m1, m2);
}

#[test]
fn test_matrix_multiply() {
    let m1 = matrix!([1, 2],
                     [3, 4]);
    let m2 = matrix!([5, 6],
                     [7, 8]);
    let actual = m1.multiply(&m2);
    let expect = matrix!([11, 22],
                         [19, 42]);
    assert_eq!(actual, expect);
}

#[test]
fn test_matrix_inverse_pass_cases() {
    {
        // Test case validating inverse of the input Matrix.
        let m = matrix!([56, 23, 98],
                        [3, 100, 200],
                        [45, 201, 123]).invert().unwrap();
        let expect = matrix!([175, 133, 33],
                             [130, 13, 245],
                             [112, 35, 126]);
        assert_eq!(m, expect);
    }
    {
        // Test case validating inverse of the input Matrix.
        let m = matrix!([1, 0, 0, 0, 0],
                        [0, 1, 0, 0 ,0],
                        [0, 0, 0, 1, 0],
                        [0, 0, 0, 0, 1],
                        [7, 7, 6, 6, 1]).invert().unwrap();
        let expect = matrix!([1, 0, 0, 0, 0],
                             [0, 1, 0, 0, 0],
                             [123, 123, 1, 122, 122],
                             [0, 0, 1, 0, 0],
                             [0, 0, 0, 1, 0]);
        assert_eq!(m, expect);
    }
}

#[test]
#[should_panic]
fn test_matrix_inverse_non_square() {
    // Test case with a non-square matrix.
    matrix!([56, 23],
            [3, 100],
            [45, 201]).invert().unwrap(); }

#[test]
#[should_panic]
fn test_matrix_inverse_singular() {
    matrix!([4, 2],
            [12, 6]).invert().unwrap(); }
