use super::matrix::Matrix;

use std::sync::RwLock;

pub struct InversionTree {
    root : RwLock<InversionNode>
}

struct InversionNode {
    matrix   : Matrix,
    children : Vec<InversionNode>
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

    fn get_inverted_matrix(&self,
                           invalid_indices : &[usize])
                           -> Matrix {
        if invalid_indices.len() {
            return self.root.read().unwrap();
        }

        {
            let mut parent = 0;
            let invalid_indices = Cell::new(invalid_indices);
        }
    }
}
