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

        self.root.read().unwrap().get_inverted_matrix(invalid_indices)
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
                                                          shards);

        Ok(())
    }
}

impl InversionNode {
    pub fn get_inverted_matrix(&self,
                               invalid_indices : &[usize])
                               -> Option<Arc<Matrix>> {
        // Set up stacks for storing the environment
        let mut node_stack            = Vec::with_capacity(1);
        let mut invalid_indices_stack = Vec::with_capacity(1);
        let mut parent_stack          = 0;

        // Initialise the stacks
        node_stack.push(self);
        invalid_indices_stack.push(invalid_indices);

        loop {
            // Extract current environment to use
            let cur_node        = node_stack.pop().unwrap();
            let invalid_indices = invalid_indices_stack.pop().unwrap();
            let parent          = parent_stack;

            // Get the child node to search next from the list of children.  The
            // list of children starts relative to the parent index passed in
            // because the indices of invalid rows is sorted (by default).  As we
            // search recursively, the first invalid index gets popped off the list,
            // so when searching through the list of children, use that first invalid
            // index to find the child node.
            let first_index = invalid_indices[0];
            let node        = &cur_node.children[first_index - parent];

            // If the child node doesn't exist in the list yet, fail fast by
            // returning, so we can construct and insert the proper inverted matrix.
            match *node {
                None           => return None,
                Some(ref node) => {
                    // If there's more than one invalid index left in the list we should
                    // keep searching recursively.
                    if invalid_indices.len() > 1 {
                        // Search recursively on the child node by passing in the invalid indices
                        // with the first index popped off the front.  Also the parent index to
                        // pass down is the first index plus one.
                        node_stack.push(node);
                        invalid_indices_stack.push(&invalid_indices[1..]);
                        parent_stack = first_index + 1;
                    } else {
                        // If there aren't any more invalid indices to search, we've found our
                        // node.  Return it, however keep in mind that the matrix could still be
                        // nil because intermediary nodes in the tree are created sometimes with
                        // their inversion matrices uninitialized.
                        return Some(Arc::clone(&node.matrix))
                    }
                }
            }
        }
    }

    pub fn insert_inverted_matrix(&mut self,
                                  invalid_indices : &[usize],
                                  matrix          : &Arc<Matrix>,
                                  shards          : usize) {
        // Set up stacks for storing the environment
        let mut node_stack            = Vec::with_capacity(1);
        let mut invalid_indices_stack = Vec::with_capacity(1);
        let mut parent_stack          = 0;

        // Initialise the stacks
        node_stack.push(self);
        invalid_indices_stack.push(invalid_indices);

        loop {
            // Extract current environment to use
            let cur_node        = node_stack.pop().unwrap();
            let invalid_indices = invalid_indices_stack.pop().unwrap();
            let parent          = parent_stack;
            // As above, get the child node to search next from the list of children.
            // The list of children starts relative to the parent index passed in
            // because the indices of invalid rows is sorted (by default).  As we
            // search recursively, the first invalid index gets popped off the list,
            // so when searching through the list of children, use that first invalid
            // index to find the child node.
            let first_index = invalid_indices[0];
            let node        = &mut cur_node.children[first_index - parent];

	          // If the child node doesn't exist in the list yet, create a new
	          // node because we have the writer lock and add it to the list
	          // of children.
            match *node {
                None => {
		                // Make the length of the list of children equal to the number
		                // of shards minus the first invalid index because the list of
		                // invalid indices is sorted, so only this length of errors
		                // are possible in the tree.
                    let mut children = Vec::with_capacity(shards - first_index);
                    for _ in 0..shards - first_index {
                        children.push(None);
                    }
                    let new_node = InversionNode {
                        matrix   : Arc::clone(matrix),
                        children
                    };
		                // Insert the new node into the tree at the first index relative
		                // to the parent index that was given in this recursive call.
                    //return cur_node.children[first_index - parent] = Some(node);
                    return *node = Some(new_node);
                },
                Some(ref mut node) => {
	                  // If there's more than one invalid index left in the list we should
	                  // keep searching recursively in order to find the node to add our
	                  // matrix.
                    if invalid_indices.len() > 1 {
		                    // As above, search recursively on the child node by passing in
		                    // the invalid indices with the first index popped off the front.
		                    // Also the total number of shards and parent index are passed down
		                    // which is equal to the first index plus one.
                        node_stack.push(node);
                        invalid_indices_stack.push(&invalid_indices[1..]);
                        parent_stack = first_index + 1;
                    } else {
		                    // If there aren't any more invalid indices to search, we've found our
		                    // node.  Cache the inverted matrix in this node.
                        return node.matrix = Arc::clone(matrix);
                    }
                }
            }
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
}
