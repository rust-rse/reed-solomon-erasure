#![allow(dead_code)]

extern crate smallvec;
use self::smallvec::SmallVec;

use galois;
#[derive(Debug)]
pub enum Error {
    SingularMatrix,
}

macro_rules! acc {
    ($m:ident, $r:expr, $c:expr)
        =>
    {
        $m.data[$r * $m.col_count + $c]
    }
}

pub fn flatten<T>(m : Vec<Vec<T>>) -> Vec<T> {
    let mut result : Vec<T> =
        Vec::with_capacity(m.len() * m[0].len());
    for row in m.into_iter() {
        for v in row.into_iter() {
            result.push(v);
        }
    }
    result
}

#[derive(PartialEq, Debug, Clone)]
pub struct Matrix {
    row_count : usize,
    col_count : usize,
    data      : SmallVec<[u8; 512]> // store in flattened structure, the smallvec can hold a 20x20 matrix in stack roughly
}

fn calc_matrix_row_start_end(col_count : usize,
                             row : usize)
                             -> (usize, usize) {
    let start = row * col_count;
    let end   = start + col_count;

    (start, end)
}

impl Matrix {
    fn calc_row_start_end(&self, row : usize) -> (usize, usize) {
        calc_matrix_row_start_end(self.col_count, row)
    }

    pub fn new(rows : usize, cols : usize) -> Matrix {
        let data = SmallVec::from_vec(vec![0; rows * cols]);

        Matrix { row_count : rows,
                 col_count : cols,
                 data }
    }

    pub fn new_with_data(init_data : Vec<Vec<u8>>) -> Matrix {
        let rows = init_data.len();
        let cols = init_data[0].len();

        for r in init_data.iter() {
            if r.len() != cols {
                panic!("Inconsistent row sizes")
            }
        }

        let data = SmallVec::from_vec(flatten(init_data));

        Matrix { row_count : rows,
                 col_count : cols,
                 data }
    }

    pub fn identity(size : usize) -> Matrix {
        let mut result = Self::new(size, size);
        for i in 0..size {
            acc!(result, i, i) = 1;
        }
        result
    }

    pub fn col_count(&self) -> usize {
        self.col_count
    }

    pub fn row_count(&self) -> usize {
        self.row_count
    }

    pub fn get(&self, r : usize, c : usize) -> u8 {
        acc!(self, r, c)
    }

    pub fn set(&mut self, r : usize, c : usize, val : u8) {
        acc!(self, r, c) = val;
    }

    pub fn multiply(&self, rhs : &Matrix) -> Matrix {
        if self.col_count != rhs.row_count {
            panic!("Colomn count on left is different from row count on right, lhs : {}, rhs : {}", self.col_count, rhs.row_count)
        }
        let mut result = Self::new(self.row_count, rhs.col_count);
        for r in 0..self.row_count {
            for c in 0..rhs.col_count {
                let mut val = 0;
                for i in 0..self.col_count {
                    val ^= galois::mul(acc!(self, r, i),
                                       acc!(rhs,  i, c));
                }
                acc!(result, r, c) = val;
            }
        }
        result
    }

    pub fn augment(&self, rhs : &Matrix) -> Matrix {
        if self.row_count != self.col_count {
            panic!("Matrices do not have the same row count, lhs : {}, rhs : {}", self.row_count, rhs.row_count)
        }
        let mut result = Self::new(self.row_count,
                                   self.col_count + rhs.col_count);
        for r in 0..self.row_count {
            for c in 0..self.col_count {
                acc!(result, r, c) = acc!(self, r, c);
            }
            let self_column_count = self.col_count;
            for c in 0..rhs.col_count {
                acc!(result, r, self_column_count + c) = acc!(rhs, r, c);
            }
        }

        result
    }

    pub fn sub_matrix(&self,
                      rmin : usize,
                      cmin : usize,
                      rmax : usize,
                      cmax : usize) -> Matrix {
        let mut result = Self::new(rmax - rmin, cmax - cmin);
        for r in rmin..rmax {
            for c in cmin..cmax {
                acc!(result, r - rmin, c - cmin) = acc!(self, r, c);
            }
        }
        result
    }

    pub fn get_row(&self, row : usize) -> &[u8] {
        let (start, end) = self.calc_row_start_end(row);

        &self.data[start..end]
    }

    pub fn swap_rows(&mut self, r1 : usize, r2 : usize) {
        let (r1_s, _) = self.calc_row_start_end(r1);
        let (r2_s, _) = self.calc_row_start_end(r2);

        if r1 == r2 {
            return;
        }
        else {
            let mut tmp;
            for i in 0..self.col_count {
                tmp = self.data[r1_s + i];
                self.data[r1_s + i] = self.data[r2_s + i];
                self.data[r2_s + i] = tmp;
            }
        }
    }

    pub fn is_square(&self) -> bool {
        self.row_count == self.col_count
    }

    pub fn gaussian_elim(&mut self) -> Result<(), Error> {
        for r in 0..self.row_count {
            if acc!(self, r, r) == 0 {
                for r_below in r+1..self.row_count {
                    if acc!(self, r_below, r) != 0 {
                        self.swap_rows(r, r_below);
                        break;
                    }
                }
            }
            // If we couldn't find one, the matrix is singular.
            if acc!(self, r, r) == 0 {
                return Err(Error::SingularMatrix)
            }
            // Scale to 1.
            if acc!(self, r, r) != 1 {
                let scale = galois::div(1, acc!(self, r, r));
                for c in 0..self.col_count {
                    acc!(self, r, c) = galois::mul(acc!(self, r, c), scale);
                }
            }
            // Make everything below the 1 be a 0 by subtracting
            // a multiple of it.  (Subtraction and addition are
            // both exclusive or in the Galois field.)
            for r_below in r+1..self.row_count {
                if acc!(self, r_below, r) != 0 {
                    let scale = acc!(self, r_below, r);
                    for c in 0..self.col_count {
                        acc!(self, r_below, c) ^=
                            galois::mul(scale,
                                        acc!(self, r, c));
                    }
                }
            }
        }

        // Now clear the part above the main diagonal.
        for d in 0..self.row_count {
            for r_above in 0..d {
                if acc!(self, r_above, d) != 0 {
                    let scale = acc!(self, r_above, d);
                    for c in 0..self.col_count {
                        acc!(self, r_above, c) ^=
                            galois::mul(scale,
                                        acc!(self, d, c));
                    }
                }
            }
        }
        Ok(())
    }

    pub fn invert(&self) -> Result<Matrix, Error> {
        if !self.is_square() {
            panic!("Trying to invert a non-square matrix")
        }

        let row_count = self.row_count;
        let col_count = self.col_count;

        let mut work = self.augment(&Self::identity(row_count));

        work.gaussian_elim()?;

        Ok(work.sub_matrix(0,
                           row_count,
                           col_count,
                           col_count * 2))
    }

    pub fn vandermonde(rows : usize, cols : usize) -> Matrix {
        let mut result = Self::new(rows, cols);

        for r in 0..rows {
            for c in 0..cols {
                acc!(result, r, c) = galois::exp(r as u8, c);
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
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

    use super::Matrix;

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
}
