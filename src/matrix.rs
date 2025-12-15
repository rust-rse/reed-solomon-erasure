#![allow(dead_code)]
extern crate alloc;

use alloc::vec;
use alloc::vec::Vec;

use crate::Field;
use smallvec::SmallVec;

const ROW_ARRAY_STACK_LENGTH: usize = 64;
/// Maximum length of the matrix data array in stack, exceeding which leads to heap allocation.
const DATA_ARRAY_STACK_LENGTH: usize = 1024;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    SingularMatrix,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Matrix<F: Field> {
    row_count: usize,
    col_count: usize,
    /// Row-major flattened matrix cells.
    data: SmallVec<[F::Elem; DATA_ARRAY_STACK_LENGTH]>,
}

type RowRef<'a, T> = &'a [T];
type RowRefArr<'a, T> = SmallVec<[RowRef<'a, T>; ROW_ARRAY_STACK_LENGTH]>;

type RowMut<'a, T> = &'a mut [T];
type RowMutArr<'a, T> = SmallVec<[RowMut<'a, T>; ROW_ARRAY_STACK_LENGTH]>;

#[derive(Debug, PartialEq, Eq)]
pub struct SubmatrixMut<'a, F: Field> {
    row_count: usize,
    col_count: usize,
    rows: RowMutArr<'a, F::Elem>,
}

impl<'a, F: Field> SubmatrixMut<'a, F> {
    fn new(row_count: usize, col_count: usize, rows: RowMutArr<'a, F::Elem>) -> Self {
        debug_assert_eq!(rows.len(), row_count);
        debug_assert!(rows.iter().all(|row| row.len() == col_count));

        Self {
            row_count,
            col_count,
            rows,
        }
    }

    /// Swap rows in place.
    fn swap_rows(&mut self, row1: usize, row2: usize) {
        let (first, second) = if row1 < row2 {
            (row1, row2)
        } else {
            (row2, row1)
        };

        let (left, right) = self.rows.split_at_mut(second);
        left[first].swap_with_slice(right[0]);
    }

    /// In-place Gaussian elimination.
    fn gaussian_elim(&mut self) -> Result<(), Error> {
        for i in 0..self.row_count {
            // Find pivot
            let mut pivot_row = None;
            for r in i..self.row_count {
                if self.rows[r][i] != F::zero() {
                    pivot_row = Some(r);
                    break;
                }
            }
            let pivot_row = pivot_row.ok_or(Error::SingularMatrix)?;

            // Swap to top
            if pivot_row != i {
                self.swap_rows(i, pivot_row);
            }

            // Scale pivot to 1
            let inv_pivot = F::inv(self.rows[i][i]);
            for a in self.rows[i].iter_mut() {
                *a = F::mul(*a, inv_pivot);
            }

            // Eliminate other rows
            for r in 0..self.row_count {
                if r != i && self.rows[r][i] != F::zero() {
                    let factor = self.rows[r][i];
                    for j in 0..self.col_count {
                        let a = self.rows[r][j];
                        self.rows[r][j] = F::add(a, F::mul(factor, self.rows[i][j]));
                    }
                }
            }
        }
        Ok(())
    }
}

impl<F: Field> Matrix<F> {
    // TODO: rename to `zero` since this in effect outputs a zero matrix.
    pub fn new(row_count: usize, col_count: usize) -> Self {
        let data = SmallVec::from_vec(vec![F::zero(); row_count * col_count]);

        Self {
            row_count,
            col_count,
            data,
        }
    }

    pub fn new_with_data(rows: &[Vec<F::Elem>]) -> Self {
        let row_count = rows.len();
        let col_count = rows[0].len();

        assert!(
            rows.iter().all(|r| r.len() == col_count),
            "Inconsistent row sizes"
        );

        let data = SmallVec::from_vec(rows.iter().flatten().copied().collect());

        Self {
            row_count,
            col_count,
            data,
        }
    }

    pub fn from_rows<'a>(rows: RowRefArr<'a, F::Elem>) -> Self {
        let row_count = rows.len();
        let col_count = rows[0].len();

        let mut data: SmallVec<[F::Elem; DATA_ARRAY_STACK_LENGTH]> =
            SmallVec::with_capacity(row_count * col_count);

        for row in rows.iter() {
            debug_assert_eq!(row.len(), col_count);
            data.extend_from_slice(row);
        }

        Self {
            row_count,
            col_count,
            data,
        }
    }

    #[cfg(test)]
    pub fn make_random(size: usize) -> Self
    where
        rand::distr::StandardUniform: rand::distr::Distribution<F::Elem>,
    {
        let mut vec: Vec<Vec<F::Elem>> = vec![vec![Default::default(); size]; size];
        for v in vec.iter_mut() {
            crate::tests::fill_random(v);
        }

        Self::new_with_data(&vec)
    }

    /// Write rows `[row_offset..]` of a Vandermonde matrix into the result. Here, `row_offset`
    /// allows to do construct submatrices of a Vandermonde matrix on which to do targeted linear
    /// algebra.
    fn vandermonde(row_count: usize, col_count: usize, row_offset: usize) -> Self {
        let mut mat = Matrix::new(row_count, col_count);

        for (i, row) in mat.rows_mut().iter_mut().enumerate() {
            // let row_gen = F::exp(F::generator(), i + 1);
            //
            // row_gen should be non-0 and unique, such as above. Below is the crate v6.0 behaviour
            // that only works, that is row_gen(0) != 0, because in that case row_offset ==
            // self.col_count != 0.
            let row_gen = F::nth(i + row_offset);
            for (j, a) in row.iter_mut().enumerate() {
                *a = F::exp(row_gen, j);
            }
        }

        mat
    }

    /// Construct the systematic generator matrix $G = [I | E]$ where $E$ is the parity encoder
    /// coefficient matrix.
    fn systematic_generator(e: Matrix<F>) -> Self {
        let col_count = e.col_count;
        let mut result = Self::new(col_count + e.row_count, col_count);
        for i in 0..col_count {
            result.set(i, i, F::one());
        }
        result.data[col_count * col_count..].copy_from_slice(&e.data);
        result
    }

    #[cfg(test)]
    pub fn identity(size: usize) -> Self {
        let mut result = Self::new(size, size);
        for i in 0..size {
            result.set(i, i, F::one());
        }
        result
    }

    pub fn col_count(&self) -> usize {
        self.col_count
    }

    pub fn row_count(&self) -> usize {
        self.row_count
    }

    pub fn get(&self, row: usize, col: usize) -> F::Elem {
        self.data[row * self.col_count + col]
    }

    pub fn set(&mut self, row: usize, col: usize, val: F::Elem) {
        self.data[row * self.col_count + col] = val;
    }

    pub fn multiply(&self, rhs: &Matrix<F>) -> Self {
        assert_eq!(
            self.col_count, rhs.row_count,
            "Colomn count on left is different from row count on right, lhs: {}, rhs: {}",
            self.col_count, rhs.row_count
        );

        let mut result = Self::new(self.row_count, rhs.col_count);
        for r in 0..self.row_count {
            for c in 0..rhs.col_count {
                let mut val = F::zero();
                for i in 0..self.col_count {
                    let mul = F::mul(self.get(r, i), rhs.get(i, c));

                    val = F::add(val, mul);
                }
                result.set(r, c, val);
            }
        }
        result
    }

    fn augment_with_identity(&self) -> Self {
        let n = self.row_count;
        let mut aug = Matrix::new(n, 2 * n);

        for i in 0..n {
            for j in 0..n {
                aug.set(i, j, self.get(i, j));
            }
            aug.set(i, n + i, F::one()); // identity
        }
        aug
    }

    fn submatrix<R, C>(&self, row_range: R, col_range: C) -> Self
    where
        R: core::ops::RangeBounds<usize>,
        C: core::ops::RangeBounds<usize>,
    {
        let b = self.matrix_bounds(row_range, col_range);

        let mut m = Matrix::new(b.row_count, b.col_count);
        for (i, r) in (b.row_start..b.row_end).enumerate() {
            for (j, c) in (b.col_start..b.col_end).enumerate() {
                m.set(i, j, self.get(r, c));
            }
        }

        m
    }

    fn submatrix_mut<'a, R, C>(&'a mut self, row_range: R, col_range: C) -> SubmatrixMut<'a, F>
    where
        R: core::ops::RangeBounds<usize>,
        C: core::ops::RangeBounds<usize>,
    {
        let b = self.matrix_bounds(row_range, col_range);
        let base_ptr = self.data.as_mut_ptr();

        let rows: RowMutArr<'a, F::Elem> = (b.row_start..b.row_end)
            .map(|i| unsafe {
                let row_ptr = base_ptr.add(i * self.col_count + b.col_start);
                core::slice::from_raw_parts_mut(row_ptr, b.col_count)
            })
            .collect();

        SubmatrixMut::new(b.row_count, b.col_count, rows)
    }

    pub fn rows<'a>(&'a self) -> RowRefArr<'a, F::Elem> {
        self.data.chunks(self.col_count).collect()
    }

    pub fn rows_mut<'a>(&'a mut self) -> RowMutArr<'a, F::Elem> {
        self.data.chunks_mut(self.col_count).collect()
    }

    pub fn get_row(&self, row: usize) -> &[F::Elem] {
        let start = row * self.col_count;
        let end = start + self.col_count;

        &self.data[start..end]
    }

    pub fn is_square(&self) -> bool {
        self.row_count == self.col_count
    }

    pub fn invert(&self) -> Result<Self, Error> {
        assert!(self.is_square(), "Trying to invert a non-square matrix");

        let mut aug = self.augment_with_identity();
        {
            let mut aug_rows = aug.submatrix_mut(.., ..);
            aug_rows.gaussian_elim()?;
        }

        Ok(aug.submatrix(0..aug.row_count, aug.row_count..aug.col_count))
    }

    fn parity_encode_coeffs(data_shards: usize, total_shards: usize) -> Self {
        let v0 = Matrix::vandermonde(data_shards, data_shards, 0);
        let v1 = Matrix::vandermonde(total_shards - data_shards, data_shards, data_shards);
        v1.multiply(&v0.invert().unwrap())
    }

    pub fn encode_coeffs(data_shards: usize, total_shards: usize) -> Self {
        let parity_encode_coeffs = Matrix::parity_encode_coeffs(data_shards, total_shards);
        Matrix::systematic_generator(parity_encode_coeffs)
    }

    fn matrix_bounds<R, C>(&self, row_range: R, col_range: C) -> MatrixBounds
    where
        R: core::ops::RangeBounds<usize>,
        C: core::ops::RangeBounds<usize>,
    {
        use core::ops::Bound::*;

        let row_start = match row_range.start_bound() {
            Included(&x) => x,
            Excluded(&x) => x + 1,
            Unbounded => 0,
        };
        let row_end = match row_range.end_bound() {
            Included(&x) => x + 1,
            Excluded(&x) => x,
            Unbounded => self.row_count,
        };

        let col_start = match col_range.start_bound() {
            Included(&x) => x,
            Excluded(&x) => x + 1,
            Unbounded => 0,
        };
        let col_end = match col_range.end_bound() {
            Included(&x) => x + 1,
            Excluded(&x) => x,
            Unbounded => self.col_count,
        };

        let row_count = row_end - row_start;
        let col_count = col_end - col_start;

        MatrixBounds {
            row_count,
            col_count,
            row_start,
            row_end,
            col_start,
            col_end,
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct MatrixBounds {
    row_count: usize,
    col_count: usize,
    row_start: usize,
    row_end: usize,
    col_start: usize,
    col_end: usize,
}

#[cfg(test)]
mod tests {
    extern crate alloc;

    use alloc::vec;

    use super::Matrix;
    use crate::galois_8;

    macro_rules! matrix {
        (
            $(
                [ $( $x:expr ),+ ]
            ),*
        ) => (
            Matrix::<galois_8::Field>::new_with_data(&[ $( vec![$( $x ),*] ),* ])
        );
        ($rows:expr, $cols:expr) => (Matrix::new($rows, $cols));
    }

    #[test]
    fn test_matrix_col_count() {
        let m1 = matrix!([1, 0, 0]);
        let m2 = matrix!([0, 0, 0], [0, 0, 0]);
        let m3: Matrix<galois_8::Field> = Matrix::new(1, 4);

        assert_eq!(3, m1.col_count());
        assert_eq!(3, m2.col_count());
        assert_eq!(4, m3.col_count());
    }

    #[test]
    fn test_matrix_row_count() {
        let m1 = matrix!([1, 0, 0]);
        let m2 = matrix!([0, 0, 0], [0, 0, 0]);
        let m3: Matrix<galois_8::Field> = Matrix::new(1, 4);

        assert_eq!(1, m1.row_count());
        assert_eq!(2, m2.row_count());
        assert_eq!(1, m3.row_count());
    }

    #[test]
    #[should_panic]
    fn test_inconsistent_row_sizes() {
        matrix!([1, 0, 0], [0, 1], [0, 0, 1]);
    }

    #[test]
    #[should_panic]
    fn test_incompatible_multiply() {
        let m1 = matrix!([0, 1], [0, 1], [0, 1]);
        let m2 = matrix!([0, 1, 2]);

        m1.multiply(&m2);
    }

    #[test]
    fn test_augment_right_identity() {
        let m1 = matrix!([0, 1], [2, 3]);

        let m2 = m1.augment_with_identity();
        let m2_right = m2.submatrix(0..2, 2..4);

        assert_eq!(m2_right, Matrix::identity(2));
    }

    #[test]
    fn test_matrix_identity() {
        let m1 = Matrix::identity(3);
        let m2 = matrix!([1, 0, 0], [0, 1, 0], [0, 0, 1]);
        assert_eq!(m1, m2);
    }

    #[test]
    fn test_matrix_multiply() {
        let m1 = matrix!([1, 2], [3, 4]);
        let m2 = matrix!([5, 6], [7, 8]);
        let actual = m1.multiply(&m2);
        let expect = matrix!([11, 22], [19, 42]);
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_matrix_inverse_pass_cases() {
        {
            // Test case validating inverse of the input Matrix.
            let m = matrix!([56, 23, 98], [3, 100, 200], [45, 201, 123])
                .invert()
                .unwrap();
            let expect = matrix!([175, 133, 33], [130, 13, 245], [112, 35, 126]);
            assert_eq!(m, expect);
        }
        {
            // Test case validating inverse of the input Matrix.
            let m = matrix!(
                [1, 0, 0, 0, 0],
                [0, 1, 0, 0, 0],
                [0, 0, 0, 1, 0],
                [0, 0, 0, 0, 1],
                [7, 7, 6, 6, 1]
            )
            .invert()
            .unwrap();
            let expect = matrix!(
                [1, 0, 0, 0, 0],
                [0, 1, 0, 0, 0],
                [123, 123, 1, 122, 122],
                [0, 0, 1, 0, 0],
                [0, 0, 0, 1, 0]
            );
            assert_eq!(m, expect);
        }
    }

    #[test]
    #[should_panic]
    fn test_matrix_inverse_non_square() {
        // Test case with a non-square matrix.
        matrix!([56, 23], [3, 100], [45, 201]).invert().unwrap();
    }

    #[test]
    #[should_panic]
    fn test_matrix_inverse_singular() {
        matrix!([4, 2], [12, 6]).invert().unwrap();
    }

    #[test]
    fn submatrix_mut_swap_rows() {
        let data: Vec<_> = (0u8..9).collect();

        let rows: Vec<Vec<u8>> = data.chunks_exact(3).map(|chunk| chunk.to_vec()).collect();

        let mut rows_rev = rows.clone();
        rows_rev.reverse();
        let m_rev = Matrix::<galois_8::Field>::new_with_data(&rows_rev);

        let mut m = Matrix::<galois_8::Field>::new_with_data(&rows);

        {
            let mut rows_mut = m.submatrix_mut(.., ..);
            rows_mut.swap_rows(0, 2);
        }

        assert_eq!(m, m_rev);
    }
}
