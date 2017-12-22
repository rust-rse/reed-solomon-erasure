use super::matrix::Matrix;

use std::sync::RwLock;

pub struct InversionTree {
    root : RwLock<InversionNode>
}

struct InversionNode {
    matrix   : Matrix,
    children : Vec<Option<InversionNode>>
}

impl InversionTree {
    fn new(data_shards : usize,
           parity_shards : usize)
           -> InversionTree {
        InversionTree {
            root : RwLock::new(InversionNode {
                matrix   : Matrix::identity(data_shards),
                children : Vec::with_capacity(data_shards + parity_shards)
            })
        }
    }

    pub fn get_inverted_matrix(&self,
                               invalid_indices : &[usize])
                               -> Option<Matrix> {
        if invalid_indices.len() {
            return self.root.read().unwrap();
        }

        {
            let mut parent = 0;
            let invalid_indices = Cell::new(invalid_indices);
        }
    }

}

impl InversionNode {
    fn get_inverted_matrix(&self,
                           invalid_indices : &[usize],
                           parent : usize)
                           -> Option<Matrix> {
        // Get the child node to search next from the list of children.  The
	      // list of children starts relative to the parent index passed in
	      // because the indices of invalid rows is sorted (by default).  As we
	      // search recursively, the first invalid index gets popped off the list,
	      // so when searching through the list of children, use that first invalid
	      // index to find the child node.
        let first_index = invalid_indices[0];
        let node        = self.children[first_index - parent];

	      // If the child node doesn't exist in the list yet, fail fast by
	      // returning, so we can construct and insert the proper inverted matrix.
        if let None = node {
            return None;
        }

	      // If there's more than one invalid index left in the list we should
	      // keep searching recursively.
        if invalid_indices.len() > 1 {
		        // Search recursively on the child node by passing in the invalid indices
		        // with the first index popped off the front.  Also the parent index to
		        // pass down is the first index plus one.
            return node.get_inverted_matrix(invalid_indices[1..],
                                            first_index + 1);
        }

	      // If there aren't any more invalid indices to search, we've found our
	      // node.  Return it, however keep in mind that the matrix could still be
	      // nil because intermediary nodes in the tree are created sometimes with
	      // their inversion matrices uninitialized.
        return node.matrix;
    }
}
