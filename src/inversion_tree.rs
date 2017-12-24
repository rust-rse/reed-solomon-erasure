use super::matrix::Matrix;

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Error {
    AlreadySet,
    NotSquare,
}

use std::sync::Mutex;
use std::sync::Arc;

#[derive(Debug)]
pub struct InversionTree {
    pub root : Mutex<InversionNode>,
    shards   : usize,
}

#[derive(Debug)]
pub struct InversionNode {
    pub matrix   : Option<Arc<Matrix>>,
    pub children : Vec<Option<InversionNode>>
}

impl InversionTree {
    pub fn new(data_shards : usize,
               parity_shards : usize)
           -> InversionTree {
        InversionTree {
            root   : Mutex::new(
                InversionNode::new(
                    Some(Arc::new(Matrix::identity(data_shards))),
                    data_shards + parity_shards)),
            shards : data_shards + parity_shards
        }
    }

    pub fn get_inverted_matrix(&self,
                               invalid_indices : &[usize])
                               -> Option<Arc<Matrix>> {
        if invalid_indices.len() == 0 {
            match self.root.lock().unwrap().matrix {
                None        => panic!(),
                Some(ref x) => return Some(Arc::clone(x))
            }
        }

        self.root.lock().unwrap().get_inverted_matrix(invalid_indices,
                                                      self.shards,
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
        self.root.lock().unwrap().insert_inverted_matrix(matrix,
                                                          invalid_indices,
                                                          shards,
                                                          0);

        Ok(())
    }
}

impl InversionNode {
    pub fn new(matrix         : Option<Arc<Matrix>>,
               children_count : usize) -> InversionNode {
        let mut children = Vec::with_capacity(children_count);
        for _ in 0..children_count {
            children.push(None);
        }
        InversionNode {
            matrix,
            children
        }
    }

    fn get_child<'a>(&'a mut self,
                     offset          : usize,
                     requested_index : usize,
                     shards          : usize)
                     -> &'a mut InversionNode {
        let node_index = requested_index - offset;
        {
            let node = &mut self.children[node_index];
            match *node {
                None    => { *node = Some(Self::new(None,
                                                    shards - offset)); },
                Some(_) => {}
            }
        }
        match self.children[node_index] {
            None            => panic!(),
            Some(ref mut x) => x
        }
    }

    pub fn get_inverted_matrix(&mut self,
                               invalid_indices : &[usize],
                               shards          : usize,
                               offset          : usize)
                               -> Option<Arc<Matrix>> {
        if invalid_indices.len() == 0 {
            match self.matrix {
                None        => None,
                Some(ref m) => Some(Arc::clone(m))
            }
        } else {
            let requested_index   = invalid_indices[0];
            let remaining_indices = &invalid_indices[1..];
            self.get_child(offset, requested_index, shards)
                .get_inverted_matrix(remaining_indices,
                                     shards,
                                     requested_index + 1)
        }
    }

    pub fn insert_inverted_matrix(&mut self,
                                  matrix          : &Arc<Matrix>,
                                  invalid_indices : &[usize],
                                  shards          : usize,
                                  offset          : usize) {
        if invalid_indices.len() == 0 {
            self.matrix = Some(Arc::clone(matrix));
        } else {
            let requested_index   = invalid_indices[0];
            let remaining_indices = &invalid_indices[1..];
            self.get_child(offset, requested_index, shards)
                .insert_inverted_matrix(matrix,
                                        remaining_indices,
                                        shards,
                                        requested_index + 1)
        }
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
        assert_eq!(expect, *tree.root.read().unwrap().matrix.unwrap());
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
