use std::sync::Arc;
use std::sync::Mutex;

use matrix::Matrix;

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Error {
    AlreadySet,
    NotSquare,
}

#[derive(Debug)]
pub struct InversionTree {
    pub root: Mutex<InversionNode>,
    total_shards: usize,
}

#[derive(Debug)]
pub struct InversionNode {
    pub matrix: Option<Arc<Matrix>>,
    pub children: Vec<Option<InversionNode>>,
}

impl InversionTree {
    pub fn new(data_shards: usize, parity_shards: usize) -> InversionTree {
        InversionTree {
            root: Mutex::new(InversionNode::new(
                Some(Arc::new(Matrix::identity(data_shards))),
                data_shards + parity_shards,
            )),
            total_shards: data_shards + parity_shards,
        }
    }

    pub fn get_inverted_matrix(&self, invalid_indices: &[usize]) -> Option<Arc<Matrix>> {
        if invalid_indices.len() == 0 {
            match self.root.lock().unwrap().matrix {
                None => panic!(),
                Some(ref x) => return Some(Arc::clone(x)),
            }
        }

        self.root
            .lock()
            .unwrap()
            .get_inverted_matrix(invalid_indices, self.total_shards, 0)
    }

    pub fn insert_inverted_matrix(
        &self,
        invalid_indices: &[usize],
        matrix: &Arc<Matrix>,
    ) -> Result<(), Error> {
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
        self.root.lock().unwrap().insert_inverted_matrix(
            matrix,
            invalid_indices,
            self.total_shards,
            0,
        );

        Ok(())
    }
}

impl InversionNode {
    pub fn new(matrix: Option<Arc<Matrix>>, children_count: usize) -> InversionNode {
        let mut children = Vec::with_capacity(children_count);
        for _ in 0..children_count {
            children.push(None);
        }
        InversionNode { matrix, children }
    }

    fn get_child<'a>(
        &'a mut self,
        offset: usize,
        requested_index: usize,
        total_shards: usize,
    ) -> &'a mut InversionNode {
        let node_index = requested_index - offset;
        {
            let node = &mut self.children[node_index];
            match *node {
                None => {
                    *node = Some(Self::new(None, total_shards - offset));
                }
                Some(_) => {}
            }
        }
        match self.children[node_index] {
            None => panic!(),
            Some(ref mut x) => x,
        }
    }

    pub fn get_inverted_matrix(
        &mut self,
        invalid_indices: &[usize],
        total_shards: usize,
        offset: usize,
    ) -> Option<Arc<Matrix>> {
        if invalid_indices.len() == 0 {
            match self.matrix {
                None => None,
                Some(ref m) => Some(Arc::clone(m)),
            }
        } else {
            let requested_index = invalid_indices[0];
            let remaining_indices = &invalid_indices[1..];
            self.get_child(offset, requested_index, total_shards)
                .get_inverted_matrix(remaining_indices, total_shards, requested_index + 1)
        }
    }

    pub fn insert_inverted_matrix(
        &mut self,
        matrix: &Arc<Matrix>,
        invalid_indices: &[usize],
        total_shards: usize,
        offset: usize,
    ) {
        if invalid_indices.len() == 0 {
            self.matrix = Some(Arc::clone(matrix));
        } else {
            let requested_index = invalid_indices[0];
            let remaining_indices = &invalid_indices[1..];
            self.get_child(offset, requested_index, total_shards)
                .insert_inverted_matrix(
                    matrix,
                    remaining_indices,
                    total_shards,
                    requested_index + 1,
                )
        }
    }
}
