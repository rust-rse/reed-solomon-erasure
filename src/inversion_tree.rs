extern crate alloc;

use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;
use core::sync::atomic::{AtomicUsize, Ordering};
#[cfg(feature = "std")]
use parking_lot::Mutex;
#[cfg(not(feature = "std"))]
use spin::Mutex;

use crate::matrix::Matrix;
use crate::Field;

const DEFAULT_INDICES_LIMIT: usize = 254;

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Error {
    AlreadySet,
    NotSquare,
}

#[derive(Debug)]
pub struct InversionTree<F: Field> {
    pub root: Mutex<InversionNode<F>>,
    total_shards: usize,
    total_indices: AtomicUsize,
    indices_limit: usize,
}

#[derive(Debug)]
pub struct InversionNode<F: Field> {
    pub matrix: Option<Arc<Matrix<F>>>,
    pub children: Vec<Option<InversionNode<F>>>,
    pub used: u64,
}

impl<F: Field> InversionTree<F> {
    pub fn new(data_shards: usize, parity_shards: usize) -> InversionTree<F> {
        Self::with_limit(data_shards, parity_shards, DEFAULT_INDICES_LIMIT)
    }

    pub fn with_limit(
        data_shards: usize,
        parity_shards: usize,
        indices_limit: usize,
    ) -> InversionTree<F> {
        InversionTree {
            root: Mutex::new(InversionNode::new(
                Some(Arc::new(Matrix::identity(data_shards))),
                data_shards + parity_shards,
            )),
            total_shards: data_shards + parity_shards,
            total_indices: AtomicUsize::new(0),
            indices_limit: indices_limit,
        }
    }

    pub fn get_inverted_matrix(&self, invalid_indices: &[usize]) -> Option<Arc<Matrix<F>>> {
        if invalid_indices.len() == 0 {
            match self.root.lock().matrix {
                None => panic!(),
                Some(ref x) => return Some(Arc::clone(x)),
            }
        }

        self.root
            .lock()
            .get_inverted_matrix(invalid_indices, self.total_shards, 0)
    }

    pub fn insert_inverted_matrix(
        &self,
        invalid_indices: &[usize],
        matrix: &Arc<Matrix<F>>,
    ) -> Result<(), Error> {
        // If no invalid indices were given then we are done because the
        // root node is already set with the identity matrix.
        if invalid_indices.len() == 0 {
            return Err(Error::AlreadySet);
        }

        if !matrix.is_square() {
            return Err(Error::NotSquare);
        }

        // https://github.com/darrenldl/reed-solomon-erasure/issues/74
        // partial solution from https://github.com/near/nearcore/pull/2317
        // suggested eviction policy: LRU
        let mut total_indices = self.total_indices.load(Ordering::Relaxed);
        total_indices += invalid_indices.len();

        if total_indices >= self.indices_limit {
            self.root.lock().evict(invalid_indices.len());
        } else {
            self.total_indices.store(total_indices, Ordering::Relaxed);
        }

        // Lock the tree for writing and reading before accessing the tree.
        // Recursively create nodes for the inverted matrix in the tree until
        // we reach the node to insert the matrix to.  We start by passing in
        // 0 as the parent index as we start at the root of the tree.
        self.root
            .lock()
            .insert_inverted_matrix(matrix, invalid_indices, self.total_shards, 0);

        Ok(())
    }
}

fn get_petals<F: Field>(node: &mut Option<InversionNode<F>>) -> Vec<&mut Option<InversionNode<F>>> {
    let mut petals = vec![];
    if let Some(some_node) = node {
        for child_node in some_node.children.iter_mut() {
            if let Some(node) = child_node {
                if node.children.is_empty() {
                    petals.push(child_node);
                } else {
                    let child_petals = get_petals(child_node);
                    petals.extend(child_petals);
                }
            }
        }
    }
    petals
}

impl<F: Field> InversionNode<F> {
    pub fn new(matrix: Option<Arc<Matrix<F>>>, children_count: usize) -> InversionNode<F> {
        let mut children = Vec::with_capacity(children_count);
        for _ in 0..children_count {
            children.push(None);
        }
        InversionNode {
            matrix,
            children,
            used: 0,
        }
    }

    fn get_child<'a>(
        &'a mut self,
        offset: usize,
        requested_index: usize,
        total_shards: usize,
    ) -> &'a mut InversionNode<F> {
        let node_index = requested_index - offset;
        {
            let node = &mut self.children[node_index];
            match *node {
                None => {
                    *node = Some(Self::new(None, total_shards - offset));
                }
                Some(_) => match self.children[node_index] {
                    None => panic!(),
                    Some(ref mut x) => {
                        x.used += 1;
                    }
                },
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
    ) -> Option<Arc<Matrix<F>>> {
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
        matrix: &Arc<Matrix<F>>,
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

    /// this function is getting very end leafs of trea
    /// removing least used one
    /// for count to clean be 0
    pub fn evict(&mut self, count: usize) {
        let mut petals = vec![];
        for child_node in self.children.iter_mut() {
            let child_petals = get_petals(child_node);
            petals.extend(child_petals);
        }
        if !petals.is_empty() {
            petals.sort_by(|a, b| {
                let a_used = match a {
                    None => 0,
                    Some(aa) => aa.used,
                };
                let b_used = match b {
                    None => 0,
                    Some(bb) => bb.used,
                };
                a_used.cmp(&b_used)
            });
            if let Some(first) = petals.first_mut() {
                **first = None;
            }
            let new_count = count - 1;
            if new_count > 0 {
                self.evict(new_count);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    extern crate alloc;

    use rand;

    use alloc::collections::BTreeMap;
    use alloc::sync::Arc;
    use alloc::vec;
    use alloc::vec::Vec;

    use crate::galois_8;
    use crate::inversion_tree::*;
    use crate::matrix::Matrix;

    use quickcheck::{Arbitrary, Gen, QuickCheck};

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
        let tree: InversionTree<galois_8::Field> = InversionTree::new(3, 2);

        let children = tree.root.lock().children.len();
        assert_eq!(5, children);

        let expect = matrix!([1, 0, 0], [0, 1, 0], [0, 0, 1]);
        assert_eq!(expect, *tree.get_inverted_matrix(&[]).unwrap());
    }

    #[test]
    fn test_get_inverted_matrix() {
        let tree: InversionTree<galois_8::Field> = InversionTree::new(3, 2);

        let matrix = &*tree.get_inverted_matrix(&[]).unwrap();

        let expect = matrix!([1, 0, 0], [0, 1, 0], [0, 0, 1]);

        assert_eq!(expect, *matrix);

        let matrix = tree.get_inverted_matrix(&[1]);
        assert_eq!(None, matrix);

        let matrix = tree.get_inverted_matrix(&[1, 2]);
        assert_eq!(None, matrix);

        let matrix = Matrix::new(3, 3);
        let matrix_copy = matrix.clone();
        tree.insert_inverted_matrix(&[1], &Arc::new(matrix))
            .unwrap();

        let cached_matrix = tree.get_inverted_matrix(&[1]).unwrap();
        assert_eq!(matrix_copy, *cached_matrix);
    }

    #[test]
    fn test_insert_inverted_matrix() {
        let tree: InversionTree<galois_8::Field> = InversionTree::new(3, 2);

        let matrix = Matrix::new(3, 3);
        let matrix_copy = matrix.clone();

        tree.insert_inverted_matrix(&[1], &Arc::new(matrix))
            .unwrap();
        tree.insert_inverted_matrix(&[], &Arc::new(matrix_copy))
            .unwrap_err();

        let matrix = Matrix::new(3, 2);
        tree.insert_inverted_matrix(&[2], &Arc::new(matrix))
            .unwrap_err();

        let matrix = Matrix::new(3, 3);
        tree.insert_inverted_matrix(&[0, 1], &Arc::new(matrix))
            .unwrap();
    }

    #[test]
    fn test_double_insert_inverted_matrix() {
        let tree: InversionTree<galois_8::Field> = InversionTree::new(3, 2);

        let matrix1 = Matrix::make_random(3);
        let matrix2 = Matrix::make_random(3);

        let matrix_copy1 = matrix1.clone();
        let matrix_copy2 = matrix2.clone();

        tree.insert_inverted_matrix(&[1], &Arc::new(matrix_copy1))
            .unwrap();
        tree.insert_inverted_matrix(&[1], &Arc::new(matrix_copy2))
            .unwrap();

        let cached_matrix = tree.get_inverted_matrix(&[1]).unwrap();
        assert_eq!(matrix2, *cached_matrix);
    }

    #[test]
    fn test_extended_inverted_matrix() {
        let tree: InversionTree<galois_8::Field> = InversionTree::new(10, 3);
        let matrix = Matrix::new(10, 10);
        let matrix_copy = matrix.clone();
        let matrix2 = matrix!(
            [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [1, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        );
        let matrix2_copy = matrix2.clone();
        let matrix3 = matrix!(
            [9, 1, 2, 3, 4, 5, 6, 7, 8, 0],
            [9, 1, 2, 3, 4, 5, 6, 7, 8, 0],
            [9, 1, 2, 3, 4, 5, 6, 7, 8, 0],
            [9, 1, 2, 3, 4, 5, 6, 7, 8, 0],
            [9, 1, 2, 3, 4, 5, 6, 7, 8, 0],
            [1, 1, 2, 3, 4, 5, 6, 7, 8, 0],
            [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [1, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            [1, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        );
        let matrix3_copy = matrix3.clone();

        tree.insert_inverted_matrix(&[1, 2], &Arc::new(matrix))
            .unwrap();

        let result = tree.get_inverted_matrix(&[1, 2]).unwrap();
        assert_eq!(matrix_copy, *result);

        tree.insert_inverted_matrix(&[1, 2, 5, 12], &Arc::new(matrix2))
            .unwrap();
        let result = tree.get_inverted_matrix(&[1, 2, 5, 12]).unwrap();
        assert_eq!(matrix2_copy, *result);

        tree.insert_inverted_matrix(&[0, 3, 4, 11], &Arc::new(matrix3))
            .unwrap();
        let result = tree.get_inverted_matrix(&[0, 3, 4, 11]).unwrap();
        assert_eq!(matrix3_copy, *result);
    }

    fn make_random_invalid_indices(data_shards: usize, parity_shards: usize) -> Vec<usize> {
        let mut invalid_count = 0;
        let mut res = Vec::new();
        for i in 0..data_shards + parity_shards {
            if rand::random::<bool>() && invalid_count < parity_shards {
                res.push(i);
                invalid_count += 1;
            }
        }
        res
    }

    #[derive(Debug, Clone)]
    struct QCTreeTestParam {
        data_shards: usize,
        parity_shards: usize,
        matrix_count: usize,
        iter_order: Vec<usize>,
        read_count: usize,
    }

    impl Arbitrary for QCTreeTestParam {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let size = g.size();

            let matrix_count = 5 + size % 100;

            let mut iter_order = Vec::with_capacity(matrix_count);
            for _ in 0..matrix_count {
                iter_order.push(rand::random::<usize>());
            }

            QCTreeTestParam {
                data_shards: 1 + size % 50,
                parity_shards: 1 + size % 50,
                matrix_count,
                iter_order,
                read_count: 2 + size % 10,
            }
        }
    }

    #[test]
    fn qc_tree_same_as_hash_map() {
        QuickCheck::new()
            .min_tests_passed(10_000)
            .tests(11_000)
            .max_tests(100_000)
            .quickcheck(qc_tree_same_as_hash_map_prop as fn(QCTreeTestParam) -> bool);
    }

    // inversion tree is functionally the same as a map
    // but more efficient
    fn qc_tree_same_as_hash_map_prop(param: QCTreeTestParam) -> bool {
        let tree: InversionTree<galois_8::Field> =
            InversionTree::new(param.data_shards, param.parity_shards);
        // The only reason to prefer `BTreeMap` over `HashMap` here is to support `no_std`
        let mut map = BTreeMap::new();

        let mut invalid_indices_set = Vec::with_capacity(param.matrix_count);

        for _ in 0..param.matrix_count {
            let invalid_indices =
                make_random_invalid_indices(param.data_shards, param.parity_shards);
            let matrix = Matrix::make_random(param.data_shards);
            match tree.insert_inverted_matrix(&invalid_indices, &Arc::new(matrix.clone())) {
                Ok(()) => {
                    map.insert(invalid_indices.clone(), matrix);
                    invalid_indices_set.push(invalid_indices);
                }
                Err(Error::AlreadySet) => {}
                Err(Error::NotSquare) => panic!(),
            }
        }

        for _ in 0..param.read_count {
            // iterate according to the provided order
            if invalid_indices_set.len() > 0 {
                for i in param.iter_order.iter() {
                    let i = i % invalid_indices_set.len();

                    let invalid_indices = &invalid_indices_set[i];

                    let matrix_in_tree = tree.get_inverted_matrix(invalid_indices).unwrap();
                    let matrix_in_map = map.get(invalid_indices).unwrap();
                    if matrix_in_tree.as_ref() != matrix_in_map {
                        return false;
                    }
                }
            }

            // iterate through the insertion order
            for ref invalid_indices in invalid_indices_set.iter() {
                let matrix_in_tree = tree.get_inverted_matrix(invalid_indices).unwrap();
                let matrix_in_map = map.get(*invalid_indices).unwrap();
                if matrix_in_tree.as_ref() != matrix_in_map {
                    return false;
                }
            }

            // iterate through the map's order
            for (ref invalid_indices, ref matrix_in_map) in map.iter() {
                let matrix_in_tree = tree.get_inverted_matrix(invalid_indices).unwrap();
                if matrix_in_tree.as_ref() != *matrix_in_map {
                    return false;
                }
            }
        }

        true
    }
}
