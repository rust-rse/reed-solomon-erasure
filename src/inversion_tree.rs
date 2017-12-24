use super::matrix::Matrix;

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Error {
    AlreadySet,
    NotSquare,
}

use std::sync::RwLock;
use std::sync::Arc;

#[derive(Debug)]
pub struct InversionTree {
    pub root : RwLock<InversionNode>
}

#[derive(Debug)]
pub struct InversionNode {
    pub matrix   : Arc<Matrix>,
    pub children : Vec<Option<InversionNode>>
}

impl InversionTree {
    pub fn new(data_shards : usize,
           parity_shards : usize)
           -> InversionTree {
        let mut children = Vec::with_capacity(data_shards + parity_shards);
        for _ in 0..data_shards + parity_shards {
            children.push(None);
        }
        InversionTree {
            root : RwLock::new(InversionNode {
                matrix   : Arc::new(Matrix::identity(data_shards)),
                children
            })
        }
    }

    pub fn get_inverted_matrix(&self,
                               invalid_indices : &[usize])
                               -> Option<Arc<Matrix>> {
        if invalid_indices.len() == 0 {
            return Some(Arc::clone(&self.root.read().unwrap().matrix));
        }

        self.root.read().unwrap().get_inverted_matrix(invalid_indices,
                                                      0)
    }

    pub fn insert_inverted_matrix(&self,
                                  invalid_indices : &[usize],
                                  matrix          : &Arc<Matrix>,
                                  shards          : usize)
                                  -> Result<(), Error> {
	      // If no invalid indices were given then we are done because the
	      // root node is already set with the identity matrix.
        if invalid_indices.len() == 0 {
            return Err(Error::AlreadySet);
        }

        if !matrix.is_square() {
            return Err(Error::NotSquare);
        }

	      // Lock the tree for writing and reading before accessing the tree.
	      // Recursively create nodes for the inverted matrix in the tree until
	      // we reach the node to insert the matrix to.  We start by passing in
	      // 0 as the parent index as we start at the root of the tree.
        self.root.write().unwrap().insert_inverted_matrix(invalid_indices,
                                                          matrix,
                                                          shards,
                                                          0);

        Ok(())
    }
}

impl InversionNode {
    pub fn get_inverted_matrix(&self,
                               invalid_indices : &[usize],
                               parent          : usize)
                               -> Option<Arc<Matrix>> {
    }

    pub fn insert_inverted_matrix(&mut self,
                                  invalid_indices : &[usize],
                                  matrix          : &Arc<Matrix>,
                                  shards          : usize,
                                  parent          : usize) {
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::matrix::Matrix;

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

        let children = tree.root.read().unwrap().children.len();
        assert_eq!(5, children);

        let expect = matrix!([1, 0, 0],
                             [0, 1, 0],
                             [0, 0, 1]);
        assert_eq!(expect, *tree.root.read().unwrap().matrix);
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
        tree.insert_inverted_matrix(&[1], &Arc::new(matrix), 5).unwrap();

        let cached_matrix = tree.get_inverted_matrix(&[1]).unwrap();
        assert_eq!(matrix_copy, *cached_matrix);
    }

    #[test]
    fn test_insert_inverted_matrix() {
        let tree = InversionTree::new(3, 2);

        let matrix = Matrix::new(3, 3);
        let matrix_copy = matrix.clone();

        tree.insert_inverted_matrix(&[1], &Arc::new(matrix), 5).unwrap();
        tree.insert_inverted_matrix(&[], &Arc::new(matrix_copy), 5).unwrap_err();

        let matrix = Matrix::new(3, 2);
        tree.insert_inverted_matrix(&[2], &Arc::new(matrix), 5).unwrap_err();

        let matrix = Matrix::new(3, 3);
        tree.insert_inverted_matrix(&[0, 1], &Arc::new(matrix), 5).unwrap();
    }

    #[test]
    fn test_double_insert_inverted_matrix() {
        let tree = InversionTree::new(3, 2);

        let matrix = Matrix::new(3, 3);
        let matrix_copy1 = matrix.clone();
        let matrix_copy2 = matrix.clone();

        tree.insert_inverted_matrix(&[1], &Arc::new(matrix), 5).unwrap();
        tree.insert_inverted_matrix(&[1], &Arc::new(matrix_copy1), 5).unwrap();

        let cached_matrix = tree.get_inverted_matrix(&[1]).unwrap();
        assert_eq!(matrix_copy2, *cached_matrix);
    }

    #[test]
    fn test_extended_inverted_matrix() {
        let tree = InversionTree::new(10, 3);
        let matrix = Matrix::new(10, 10);
        let matrix_copy = matrix.clone();
        
        tree.insert_inverted_matrix(&[1, 2], &Arc::new(matrix), 13).unwrap();
        
        //let result = tree.get_inverted_matrix(&[1, 2]).unwrap();
        //assert_eq!(matrix_copy, *result);
    }
}
