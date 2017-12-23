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
                        parent_stack = parent + 1;
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

    pub fn insert_inverted_matrix(&mut self,
                                  invalid_indices : &[usize],
                                  matrix          : Matrix,
                                  shards          : usize,
                                  parent          : usize) {
        let mut node_stack            = Vec::with_capacity(1);
        let mut invalid_indices_stack = Vec::with_capacity(1);
        let mut parent_stack          = 0;
        node_stack.push(self);
        invalid_indices_stack.push(invalid_indices);
        loop {
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
                        matrix   : Arc::new(matrix),
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
                        return node.matrix = Arc::new(matrix);
                    }
                }
            }
        }
    }
}
