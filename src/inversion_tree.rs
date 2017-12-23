use super::matrix::Matrix;

use std::sync::RwLock;
use std::cell::Cell;
use std::sync::Arc;

pub struct InversionTree {
    root : RwLock<InversionNode>
}

struct InversionNode {
    matrix   : Arc<Matrix>,
    children : Vec<Option<InversionNode>>
}

impl InversionTree {
    fn new(data_shards : usize,
           parity_shards : usize)
           -> InversionTree {
        InversionTree {
            root : RwLock::new(InversionNode {
                matrix   : Arc::new(Matrix::identity(data_shards)),
                children : Vec::with_capacity(data_shards + parity_shards)
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

}

impl InversionNode {
    pub fn get_inverted_matrix(&self,
                               invalid_indices : &[usize])
                               -> Option<Arc<Matrix>> {
        let mut node_stack            = Vec::with_capacity(1);
        let mut invalid_indices_stack = Vec::with_capacity(1);
        let mut parent_stack      = 0;
        node_stack.push(self);
        invalid_indices_stack.push(invalid_indices);
        loop {
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
                        parent_stack += 1;
                    }

                    // If there aren't any more invalid indices to search, we've found our
                    // node.  Return it, however keep in mind that the matrix could still be
                    // nil because intermediary nodes in the tree are created sometimes with
                    // their inversion matrices uninitialized.
                    return Some(Arc::clone(&node.matrix))
                }
            }
        }
    }

    pub fn insert_inverted_matrix(&self,
                                  invalid_indices : &[usize],
                                  matrix          : Matrix,
                                  shards          : usize,
                                  parent          : usize) {
	      // As above, get the child node to search next from the list of children.
	      // The list of children starts relative to the parent index passed in
	      // because the indices of invalid rows is sorted (by default).  As we
	      // search recursively, the first invalid index gets popped off the list,
	      // so when searching through the list of children, use that first invalid
	      // index to find the child node.
        let first_index = invalid_indices[0];
        let node        = &self.children[first_index - parent];

    }
}
