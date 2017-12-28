//! This crate provides an encoder/decoder for Reed-Solomon erasure code.
//!
//! Please note that erasure coding means errors are not directly detected or corrected,
//! but missing data pieces(shards) can be reconstructed given that
//! the configuration provides high enough redundancy.
//!
//! You will have to implement error detection separately(e.g. via checksums)
//! and simply leave out the corrupted shards when attempting to reconstruct
//! the missing data.

//#![allow(dead_code)]
mod misc_utils;
mod galois;
mod matrix;
mod inversion_tree;
mod shard_utils;
mod lib_test;

pub use shard_utils::make_zero_len_shard;
pub use shard_utils::make_zero_len_shards;
pub use shard_utils::make_blank_shard;
pub use shard_utils::make_blank_shards;
pub use shard_utils::shards_to_option_shards;
pub use shard_utils::shards_into_option_shards;
pub use shard_utils::option_shards_to_shards;
pub use shard_utils::option_shards_into_shards;

extern crate rayon;
use rayon::prelude::*;
use std::sync::Arc;

extern crate smallvec;
use smallvec::SmallVec;

use matrix::Matrix;
use inversion_tree::InversionTree;

#[derive(PartialEq, Debug)]
pub enum Error {
    TooFewShards,
    TooManyShards,
    IncorrectShardSize,
    TooFewShardsPresent,
    EmptyShard,
    InvalidShardsIndicator,
    InversionTreeError(inversion_tree::Error)
}

/// Convenience data type provided by this library.
pub type Shard = Box<[u8]>;

/// Constructs a shard.
///
/// # Example
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # use reed_solomon_erasure::*;
/// # fn main () {
/// let shard = shard![1, 2, 3];
/// # }
/// ```
#[macro_export]
macro_rules! shard {
    [
        $( $x:expr ),*
    ] => {{
        vec![ $( $x as u8 ),* ].into_boxed_slice()
    }}
}

/// Constructs vector of shards.
///
/// # Example
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # use reed_solomon_erasure::*;
/// # fn main () {
/// let shards = shards!([1, 2, 3],
///                      [4, 5, 6]);
/// # }
/// ```
#[macro_export]
macro_rules! shards {
    (
        $( [ $( $x:expr ),* ] ),*
    ) => {{
        vec![ $( vec![ $( $x as u8),* ].into_boxed_slice() ),* ]
    }}
}

/// Makes it easier to work with 2D slices, arrays, etc.
///
/// # Examples
/// ## Byte arrays on stack to `Vec<&[u8]>`
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # fn main () {
/// let array : [[u8; 3]; 2] = [[1, 2, 3],
///                             [4, 5, 6]];
///
/// let refs : Vec<&[u8]> =
///     convert_2D_slices!(array =to_vec=> &[u8]);
/// # }
/// ```
/// ## Byte arrays on stack to `Vec<&mut [u8]>` (borrow mutably)
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # fn main () {
/// let mut array : [[u8; 3]; 2] = [[1, 2, 3],
///                                 [4, 5, 6]];
///
/// let refs : Vec<&mut [u8]> =
///     convert_2D_slices!(array =to_mut_vec=> &mut [u8]);
/// # }
/// ```
/// ## Byte arrays on stack to `SmallVec<[&mut [u8]; 32]>` (borrow mutably)
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # extern crate smallvec;
/// # use smallvec::SmallVec;
/// # fn main () {
/// let mut array : [[u8; 3]; 2] = [[1, 2, 3],
///                                 [4, 5, 6]];
///
/// let refs : SmallVec<[&mut [u8]; 32]> =
///     convert_2D_slices!(array =to_mut=> SmallVec<[&mut [u8]; 32]>,
///                        SmallVec::with_capacity);
/// # }
/// ```
/// ## Shard array to `SmallVec<[&mut [u8]; 32]>` (borrow mutably)
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # extern crate smallvec;
/// # use smallvec::SmallVec;
/// # fn main () {
/// let mut shards = shards!([1, 2, 3],
///                          [4, 5, 6]);
///
/// let refs : SmallVec<[&mut [u8]; 32]> =
///     convert_2D_slices!(shards =to_mut=> SmallVec<[&mut [u8]; 32]>,
///                        SmallVec::with_capacity);
/// # }
/// ```
/// ## Shard array to `Vec<&mut [u8]>` (borrow mutably) into `SmallVec<[&mut [u8]; 32]>` (move)
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # extern crate smallvec;
/// # use smallvec::SmallVec;
/// # fn main () {
/// let mut shards = shards!([1, 2, 3],
///                          [4, 5, 6]);
///
/// let refs1 = convert_2D_slices!(shards =to_mut_vec=> &mut [u8]);
///
/// let refs2 : SmallVec<[&mut [u8]; 32]> =
///     convert_2D_slices!(refs1 =into=> SmallVec<[&mut [u8]; 32]>,
///                        SmallVec::with_capacity);
/// # }
/// ```
#[macro_export]
macro_rules! convert_2D_slices {
    (
        $slice:ident =into_vec=> $dst_type:ty
    ) => {
        convert_2D_slices!($slice =into=> Vec<$dst_type>,
                           Vec::with_capacity)
    };
    (
        $slice:ident =to_vec=> $dst_type:ty
    ) => {
        convert_2D_slices!($slice =to=> Vec<$dst_type>,
                           Vec::with_capacity)
    };
    (
        $slice:ident =to_mut_vec=> $dst_type:ty
    ) => {
        convert_2D_slices!($slice =to_mut=> Vec<$dst_type>,
                           Vec::with_capacity)
    };
    (
        $slice:ident =into=> $dst_type:ty, $with_capacity:path
    ) => {{
        let mut result : $dst_type =
            $with_capacity($slice.len());
        for i in $slice.into_iter() {
            result.push(i);
        }
        result
    }};
    (
        $slice:ident =to=> $dst_type:ty, $with_capacity:path
    ) => {{
        let mut result : $dst_type =
            $with_capacity($slice.len());
        for i in $slice.iter() {
            result.push(i);
        }
        result
    }};
    (
        $slice:ident =to_mut=> $dst_type:ty, $with_capacity:path
    ) => {{
        let mut result : $dst_type =
            $with_capacity($slice.len());
        for i in $slice.iter_mut() {
            result.push(i);
        }
        result
    }}
}

fn shards_to_slices<'a>(shards : &'a [Shard]) -> Vec<&'a [u8]> {
    let mut result : Vec<&[u8]> =
        Vec::with_capacity(shards.len());
    for shard in shards.into_iter() {
        result.push(shard);
    }
    result
}

/*fn mut_shards_to_mut_slices(shards : &mut [Shard])
                            -> Vec<&mut [u8]> {
    let mut result : Vec<&mut [u8]> =
        Vec::with_capacity(shards.len());
    for shard in shards.into_iter() {
        result.push(shard);
    }
    result
}*/

fn mut_option_shards_to_mut_slices<'a>(shards : &'a mut [Option<Shard>])
                                       -> Vec<&'a mut [u8]> {
    let mut result : Vec<&mut [u8]> =
        Vec::with_capacity(shards.len());
    for shard in shards.iter_mut() {
        match *shard {
            None => panic!("Option shards slot is None"),
            Some(ref mut s) => {
                result.push(s);
            }
        }
    }
    result
}

/*mod helper {
    pub fn calc_slice_start_end(i : usize, chunk_size : usize)
                                -> (usize, usize) {
        let s = i * chunk_size;
        let e = s + chunk_size - 1;
        (s, e)
    }
}*/

/// Reed-Solomon erasure code encoder/decoder.
///
/// # Common error handling
///
/// ### For `encode`, `encode_shards`, `verify`, `verify_shards`, `reconstruct`, `reconstruct_data`, `reconstruct_shards`, `reconstruct_data_shards`
///
/// Return `Error::TooFewShards` or `Error::TooManyShards`
/// when the number of provided shards
/// does not match the codec's one.
///
/// Return `Error::EmptyShard` when the first shard provided is
/// of zero length.
///
/// Return `Error::IncorrectShardSize` when the provided shards
/// are of different length.
///
/// ### For `reconstruct`, `reconstruct_data`, `reconstruct_shards`, `reconstruct_data_shards`
///
/// Return `Error::TooFewShardsPresent` when there are not
/// enough shards for reconstruction.
#[derive(Debug)]
pub struct ReedSolomon {
    data_shard_count   : usize,
    parity_shard_count : usize,
    total_shard_count  : usize,
    matrix             : Matrix,
    tree               : InversionTree,
    //parity_rows        : Vec<Vec<[u8]>>,
    pparam             : ParallelParam
}

/// Parameters for parallelism.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct ParallelParam {
    /// Number of bytes to split the slices into for computations
    /// which can be done in parallel.
    ///
    /// Default is 8192.
    pub bytes_per_encode  : usize,
    //pub shards_per_encode : usize,
}

impl ParallelParam {
    pub fn new(bytes_per_encode  : usize,
               /*shards_per_encode : usize*/) -> ParallelParam {
        ParallelParam { bytes_per_encode,
                        /*shards_per_encode*/ }
    }

    pub fn with_default() -> ParallelParam {
        Self::new(8192,
                  /*4*/)
    }
}

impl Clone for ReedSolomon {
    fn clone(&self) -> ReedSolomon {
        ReedSolomon::with_pparam(self.data_shard_count,
                                 self.parity_shard_count,
                                 self.pparam.clone())
    }
}

impl PartialEq for ReedSolomon {
    fn eq(&self, rhs : &ReedSolomon) -> bool {
        self.data_shard_count == rhs.data_shard_count
            && self.parity_shard_count == rhs.parity_shard_count
            && self.total_shard_count  == rhs.total_shard_count
            && self.matrix             == rhs.matrix
            && self.pparam             == rhs.pparam
    }
}

macro_rules! check_piece_count {
    (
        $self:ident, $pieces:ident
    ) => {{
        if $pieces.len() < $self.total_shard_count {
            return Err(Error::TooFewShards);
        }
        if $pieces.len() > $self.total_shard_count {
            return Err(Error::TooManyShards);
        }
    }}
}

macro_rules! check_slices {
    (
        $slices:ident
    ) => {{
        let size = $slices[0].len();
        if size == 0 {
            return Err(Error::EmptyShard);
        }
        for slice in $slices.iter() {
            if slice.len() != size {
                return Err(Error::IncorrectShardSize);
            }
        }
    }}
}

impl ReedSolomon {
    fn get_parity_rows(&self) -> SmallVec<[&[u8]; 32]> {
        let mut parity_rows  = SmallVec::with_capacity(self.parity_shard_count);
        let matrix           = &self.matrix;
        for i in self.data_shard_count..self.total_shard_count {
            parity_rows.push(matrix.get_row(i));
        }

        parity_rows
    }

    fn build_matrix(data_shards : usize, total_shards : usize) -> Matrix {
        let vandermonde = Matrix::vandermonde(total_shards, data_shards);

        let top = vandermonde.sub_matrix(0, 0, data_shards, data_shards);

        vandermonde.multiply(&top.invert().unwrap())
    }

    /// Creates a new instance of Reed-Solomon erasure code encoder/decoder.
    pub fn new(data_shards : usize,
               parity_shards : usize) -> ReedSolomon {
        Self::with_pparam(data_shards,
                          parity_shards,
                          ParallelParam::with_default())
    }

    /// Creates a new instance of Reed-Solomon erasure code encoder/decoder with custom `ParallelParam`.
    pub fn with_pparam(data_shards   : usize,
                       parity_shards : usize,
                       pparam        : ParallelParam) -> ReedSolomon {
        if data_shards == 0 {
            panic!("Too few data shards")
        }
        if parity_shards == 0 {
            panic!("Too few pairty shards")
        }
        if 256 < data_shards + parity_shards {
            panic!("Too many shards, max is 256")
        }

        let total_shards    = data_shards + parity_shards;

        let matrix = Self::build_matrix(data_shards, total_shards);

        ReedSolomon {
            data_shard_count   : data_shards,
            parity_shard_count : parity_shards,
            total_shard_count  : total_shards,
            matrix,
            tree               : InversionTree::new(data_shards, parity_shards),
            pparam
        }
    }

    pub fn data_shard_count(&self) -> usize {
        self.data_shard_count
    }

    pub fn parity_shard_count(&self) -> usize {
        self.parity_shard_count
    }

    pub fn total_shard_count(&self) -> usize {
        self.total_shard_count
    }

    fn code_some_slices(&self,
                        matrix_rows  : &[&[u8]],
                        inputs       : &[&[u8]],
                        outputs      : &mut [&mut [u8]]) {
        for c in 0..self.data_shard_count {
            let input = inputs[c];
            misc_utils::breakdown_slice_mut_with_index
                (outputs)
                .into_par_iter()
                .for_each(|(i_row, output)| {
                    if c == 0 {
                        if output.len() <= self.pparam.bytes_per_encode {
                            galois::mul_slice(matrix_rows[i_row][c],
                                              input,
                                              output);
                        } else {
                            misc_utils::split_slice_mut_with_index(
                                output, self.pparam.bytes_per_encode)
                                .into_par_iter()
                                .for_each(|(i, output)| {
                                    let start =
                                        i * self.pparam.bytes_per_encode;
                                    galois::mul_slice(matrix_rows[i_row][c],
                                                      &input[start..start + output.len()],
                                                      output);
                                })
                        }
                    } else {
                        if output.len() <= self.pparam.bytes_per_encode {
                            galois::mul_slice_xor(matrix_rows[i_row][c],
                                                  input,
                                                  output);
                        } else {
                            misc_utils::split_slice_mut_with_index(
                                output, self.pparam.bytes_per_encode)
                                .into_par_iter()
                                .for_each(|(i, output)| {
                                    let start =
                                        i * self.pparam.bytes_per_encode;
                                    galois::mul_slice_xor(matrix_rows[i_row][c],
                                                          &input[start..start + output.len()],
                                                          output);
                                })
                        }
                    }
                })
        }
    }

    fn check_some_slices(&self,
                         matrix_rows  : &[&[u8]],
                         inputs       : &[&[u8]],
                         to_check     : &[&[u8]])
                         -> bool {
        let mut outputs =
            make_blank_shards(inputs[0].len(), to_check.len());
        for c in 0..self.data_shard_count {
            let input = inputs[c];
            misc_utils::breakdown_slice_mut_with_index
                (&mut outputs)
                .into_par_iter()
                .for_each(|(i_row, output)| {
                    misc_utils::split_slice_mut_with_index
                        (output, self.pparam.bytes_per_encode)
                        .into_par_iter()
                        .for_each(|(i, output)| {
                            let start =
                                i * self.pparam.bytes_per_encode;
                            galois::mul_slice_xor(matrix_rows[i_row][c],
                                                  &input[start..start + output.len()],
                                                  output);
                        })
                })
        }
        let any_shard_mismatch = misc_utils::breakdown_slice_with_index
            (&outputs)
            .into_par_iter()
            .map(|(i, output)| {
                let to_check = to_check[i];
                misc_utils::split_slice_with_index
                    (output, self.pparam.bytes_per_encode)
                    .into_par_iter()
                    .map(|(i, output)| {
                        let start =
                            i * self.pparam.bytes_per_encode;
                        // the following returns true if the chunks match
                        misc_utils::slices_are_equal(output, &to_check[start..start + output.len()])
                    })
                    .any(|x| !x)  // find the first false(some chunks are inequal), which will cause this to return true
            })
            .any(|x| x);  // find the first true(some chunks are inequal)
        !any_shard_mismatch  // if it is not that case that any of the shard has a mismatch
    }

    fn option_shards_size(slices : &[Option<Shard>]) -> Result<usize, Error> {
        let mut size = None;
        for slice in slices.iter() {
            match *slice {
                None => {},
                Some(ref s) => {
                    size = Some(s.len());
                    break;
                }
            }
        }
        match size {
            None       => Err(Error::EmptyShard),
            Some(size) => Ok(size)
        }
    }

    fn check_option_shards(slices : &[Option<Shard>]) -> Result<(), Error> {
        let mut size = None;
        for slice in slices.iter() {
            match *slice {
                None => {},
                Some(ref s) => {
                    size = Some(s.len());
                    break;
                }
            }
        }
        match size {
            None    => return Err(Error::TooFewShardsPresent),
            Some(size) => {
                if size == 0 {
                    return Err(Error::EmptyShard);
                }
                for slice in slices.iter() {
                    match *slice {
                        None => {},
                        Some(ref slice) => {
                            if slice.len() != size {
                                return Err(Error::IncorrectShardSize);
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Constructs the parity shards.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    /// This is a wrapper of `encode`.
    pub fn encode_shards(&self,
                         shards : &mut [Shard]) -> Result<(), Error> {
        let mut slices : SmallVec<[&mut [u8]; 32]> =
            convert_2D_slices!(shards =into=> SmallVec<[&mut [u8]; 32]>,
                               SmallVec::with_capacity);

        self.encode(&mut slices)
    }

    /// Constructs the parity shards.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    pub fn encode(&self,
                  slices : &mut [&mut [u8]]) -> Result<(), Error> {
        check_piece_count!(self, slices);

        check_slices!(slices);

        let parity_rows = self.get_parity_rows();

	      // Get the slice of output buffers.
        let (mut_input, output) =
            slices.split_at_mut(self.data_shard_count);

        let input =
            convert_2D_slices!(mut_input =into=> SmallVec<[&[u8]; 32]>,
                               SmallVec::with_capacity);

	      // Do the coding.
        self.code_some_slices(&parity_rows,
                              &input,
                              output);

        Ok(())
    }

    /// Checks if the parity shards are correct.
    ///
    /// This is a wrapper of `verify`.
    pub fn verify_shards(&self,
                         shards : &[Shard]) -> Result<bool, Error> {
        let slices =
            shards_to_slices(shards);

        self.verify(&slices)
    }

    /// Checks if the parity shards are correct.
    pub fn verify(&self,
                  slices : &[&[u8]]) -> Result<bool, Error> {
        check_piece_count!(self, slices);

        check_slices!(slices);

        let to_check = &slices[self.data_shard_count..];

        let parity_rows = self.get_parity_rows();

        Ok(self.check_some_slices(&parity_rows,
                                  &slices[0..self.data_shard_count],
                                  to_check))
    }

    /// Reconstructs all shards.
    ///
    /// `reconstruct`, `reconstruct_data`, `reconstruct_shards`,
    /// `reconstruct_data_shards` share the same core code base.
    pub fn reconstruct(&self,
                       slices        : &mut [&mut [u8]],
                       slice_present : &[bool]) -> Result<(), Error> {
        self.reconstruct_internal(slices,
                                  slice_present,
                                  false)
    }

    /// Reconstructs only the data shards.
    ///
    /// `reconstruct`, `reconstruct_data`, `reconstruct_shards`,
    /// `reconstruct_data_shards` share the same core code base.
    pub fn reconstruct_data(&self,
                            slices        : &mut [&mut [u8]],
                            slice_present : &[bool]) -> Result<(), Error> {
        self.reconstruct_internal(slices,
                                  slice_present,
                                  true)
    }

    /// Reconstructs all shards.
    ///
    /// This fills in the missing shards with blank shards only
    /// if there are enough shards for reconstruction.
    ///
    /// `reconstruct`, `reconstruct_data`, `reconstruct_shards`,
    /// `reconstruct_data_shards` share the same core code base.
    pub fn reconstruct_shards(&self,
                              shards : &mut [Option<Shard>]) -> Result<(), Error> {
        self.reconstruct_shards_internal(shards, false)
    }

    /// Reconstructs only the data shards.
    ///
    /// This fills in the missing shards with blank shards only
    /// if there are enough shards for reconstruction.
    ///
    /// Note that the missing parity shards are filled in with
    /// blank shards even though they are not used.
    ///
    /// `reconstruct`, `reconstruct_data`, `reconstruct_shards`,
    /// `reconstruct_data_shards` share the same core code base.
    pub fn reconstruct_data_shards(&self,
                                   shards : &mut [Option<Shard>]) -> Result<(), Error> {
        self.reconstruct_shards_internal(shards, true)
    }

    fn reconstruct_shards_internal(&self,
                                   shards    : &mut [Option<Shard>],
                                   data_only : bool)
                                   -> Result<(), Error> {
        check_piece_count!(self, shards);

        Self::check_option_shards(shards)?;

        let shard_size = Self::option_shards_size(shards).unwrap();

	      // Quick check: are all of the shards present?  If so, there's
	      // nothing to do.
        let mut number_present = 0;
        let mut shard_present : SmallVec<[bool; 32]> =
            SmallVec::with_capacity(shards.len());
        for shard in shards.iter() {
            match *shard {
                None    => { shard_present.push(false); },
                Some(_) => { number_present += 1;
                             shard_present.push(true); }
            }
        }
        if number_present == self.total_shard_count {
            // Cool.  All of the shards data data.  We don't
            // need to do anything.
            return Ok(())
        }

	      // More complete sanity check
	      if number_present < self.data_shard_count {
		        return Err(Error::TooFewShardsPresent)
	      }

        // Fill in new shards
        for i in 0..shards.len() {
            if !shard_present[i] {
                shards[i] = Some(make_blank_shard(shard_size));
            }
        }

        let mut slices =
            mut_option_shards_to_mut_slices(shards);

        self.reconstruct_internal(&mut slices,
                                  &shard_present,
                                  data_only)
    }

    fn get_data_decode_matrix(&self,
                              valid_indices : &[usize],
                              invalid_indices : &[usize])
                              -> Result<Arc<Matrix>, Error> {
        // Attempt to get the cached inverted matrix out of the tree
        // based on the indices of the invalid rows.
        match self.tree.get_inverted_matrix(&invalid_indices) {
            // If the inverted matrix isn't cached in the tree yet we must
            // construct it ourselves and insert it into the tree for the
            // future.  In this way the inversion tree is lazily loaded.
            None => {
                // Pull out the rows of the matrix that correspond to the
                // shards that we have and build a square matrix.  This
                // matrix could be used to generate the shards that we have
                // from the original data.
                let mut sub_matrix =
                    Matrix::new(self.data_shard_count,
                                self.data_shard_count);
                for sub_matrix_row in 0..valid_indices.len() {
                    let valid_index = valid_indices[sub_matrix_row];
                    for c in 0..self.data_shard_count {
                        sub_matrix.set(sub_matrix_row, c,
                                       self.matrix.get(valid_index, c));
                    }
                }
                // Invert the matrix, so we can go from the encoded shards
                // back to the original data.  Then pull out the row that
                // generates the shard that we want to decode.  Note that
                // since this matrix maps back to the original data, it can
                // be used to create a data shard, but not a parity shard.
                let data_decode_matrix = Arc::new(sub_matrix.invert().unwrap());

                // Cache the inverted matrix in the tree for future use keyed on the
                // indices of the invalid rows.
                let insert_result =
                    self.tree.insert_inverted_matrix(&invalid_indices,
                                                     &data_decode_matrix,
                                                     self.total_shard_count);
                match insert_result {
                    Ok(()) => {},
                    Err(x) => return Err(Error::InversionTreeError(x))
                }
                Ok(data_decode_matrix)
            },
            Some(m) => {
                Ok(m)
            }
        }
    }

    fn reconstruct_internal(&self,
                            slices        : &mut [&mut [u8]],
                            slice_present : &[bool],
                            data_only     : bool) -> Result<(), Error> {
        check_piece_count!(self, slices);

        check_slices!(slices);

        if slices.len() != slice_present.len() {
            return Err(Error::InvalidShardsIndicator);
        }

	      // Quick check: are all of the shards present?  If so, there's
	      // nothing to do.
        let mut number_present = 0;
        for i in 0..slices.len() {
            if slice_present[i] {
                number_present += 1;
            }
        }
        if number_present == self.total_shard_count {
            // Cool.  All of the shards are there.  We don't
            // need to do anything.
            return Ok(())
        }

	      // More complete sanity check
	      if number_present < self.data_shard_count {
		        return Err(Error::TooFewShardsPresent)
	      }

	      // Pull out an array holding just the shards that
	      // correspond to the rows of the submatrix.  These shards
	      // will be the input to the decoding process that re-creates
	      // the missing data shards.
	      //
	      // Also, create an array of indices of the valid rows we do have
	      // and the invalid rows we don't have up until we have enough valid rows.
        let mut sub_shards             : SmallVec<[&[u8];     32]> =
            SmallVec::with_capacity(self.data_shard_count);
        let mut leftover_parity_shards : SmallVec<[&[u8];     32]> =
            SmallVec::with_capacity(self.parity_shard_count);
        let mut missing_data_slices    : SmallVec<[&mut [u8]; 32]> =
            SmallVec::with_capacity(self.parity_shard_count);
        let mut missing_parity_slices  : SmallVec<[&mut [u8]; 32]> =
            SmallVec::with_capacity(self.parity_shard_count);
        let mut valid_indices          : SmallVec<[usize;     32]> =
            SmallVec::with_capacity(self.data_shard_count);
        let mut invalid_indices        : SmallVec<[usize;     32]> =
            SmallVec::with_capacity(self.data_shard_count);
        let mut matrix_row = 0;
        let mut i          = 0;
        // Separate the slices into groups
        for slice in slices.into_iter() {
            if slice_present[i] {
                if sub_shards.len() < self.data_shard_count {
                    sub_shards.push(slice);
                    valid_indices.push(matrix_row);
                } else {
                    leftover_parity_shards.push(slice);
                }
            } else {
                if i < self.data_shard_count {
                    missing_data_slices.push(slice);
                } else {
                    missing_parity_slices.push(slice);
                }
                invalid_indices.push(matrix_row);
            }

            i          += 1;
            matrix_row += 1;
        }

        let data_decode_matrix =
            self.get_data_decode_matrix(&valid_indices,
                                        &invalid_indices)?;

        // Re-create any data shards that were missing.
        //
        // The input to the coding is all of the shards we actually
        // have, and the output is the missing data shards.  The computation
        // is done using the special decode matrix we just built.
        let mut matrix_rows = Vec::with_capacity(self.parity_shard_count);
        for i_slice in 0..self.data_shard_count {
            if !slice_present[i_slice] {
                matrix_rows.push(data_decode_matrix.get_row(i_slice));
            }
        }
        self.code_some_slices(&matrix_rows,
                              &sub_shards,
                              &mut missing_data_slices);

        if data_only {
            Ok(())
        } else {
	          // Now that we have all of the data shards intact, we can
	          // compute any of the parity that is missing.
	          //
	          // The input to the coding is ALL of the data shards, including
	          // any that we just calculated.  The output is whichever of the
	          // parity shards were missing.
            let mut matrix_rows : SmallVec<[&[u8]; 32]> =
                SmallVec::with_capacity(self.parity_shard_count);
            let parity_rows = self.get_parity_rows();
            for i_slice in self.data_shard_count..self.total_shard_count {
                if !slice_present[i_slice] {
                    matrix_rows.push(
                        parity_rows[i_slice
                                    - self.data_shard_count]);
                }
            }
            {
                // Gather up the references to all data slices
                let mut i_old_data_slice = 0;
                let mut i_new_data_slice = 0;
                let mut all_data_slices : SmallVec<[&[u8]; 32]> =
                    SmallVec::with_capacity(self.data_shard_count);
                for i_slice in 0..self.data_shard_count {
                    let slice =
                        if slice_present[i_slice] {
                            let result = sub_shards[i_old_data_slice];
                            i_old_data_slice += 1;
                            result
                        } else {
                            let result = &missing_data_slices[i_new_data_slice];
                            i_new_data_slice += 1;
                            result
                        };
                    all_data_slices.push(slice);
                }
                // Now do the actual computation for the missing
                // parity shards
                self.code_some_slices(&matrix_rows,
                                      &all_data_slices,
                                      &mut missing_parity_slices);
            }

            Ok(())
        }
    }
}
