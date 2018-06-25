//! This crate provides an encoder/decoder for Reed-Solomon erasure code.
//!
//! Please note that erasure coding means errors are not directly detected or corrected,
//! but missing data pieces(shards) can be reconstructed given that
//! the configuration provides high enough redundancy.
//!
//! You will have to implement error detection separately(e.g. via checksums)
//! and simply leave out the corrupted shards when attempting to reconstruct
//! the missing data.

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

#[cfg(test)]
extern crate rand;

extern crate rayon;
use rayon::prelude::*;
use std::sync::Arc;

extern crate smallvec;
use smallvec::SmallVec;

extern crate libc;

#[macro_use]
mod macros;

mod misc_utils;
mod galois;
mod galois_tests;
mod matrix;
mod matrix_tests;
mod inversion_tree;
mod inversion_tree_tests;
mod shard_utils;
mod lib_tests;
mod errors;
mod errors_tests;

pub use errors::Error;
pub use errors::SBSError;

pub use shard_utils::make_zero_len_shard;
pub use shard_utils::make_zero_len_shards;
pub use shard_utils::make_blank_shard;
pub use shard_utils::make_blank_shards;
pub use shard_utils::shards_to_option_shards;
pub use shard_utils::shards_into_option_shards;
pub use shard_utils::option_shards_to_shards;
pub use shard_utils::option_shards_into_shards;

use matrix::Matrix;
use inversion_tree::InversionTree;

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

/// Reed-Solomon erasure code encoder/decoder.
///
/// # Common error handling
///
/// ## For `encode`, `encode_shards`, `verify`, `verify_shards`, `reconstruct`, `reconstruct_data`, `reconstruct_shards`, `reconstruct_data_shards`
///
/// Return `Error::TooFewShards` or `Error::TooManyShards`
/// when the number of provided shards
/// does not match the codec's one.
///
/// Return `Error::EmptyShard` when the first shard provided is
/// of zero length.
///
/// Return `Error::IncorrectShardSize` when the provided shards
/// are of different lengths.
///
/// ## For `reconstruct`, `reconstruct_data`, `reconstruct_shards`, `reconstruct_data_shards`
///
/// Return `Error::TooFewShardsPresent` when there are not
/// enough shards for reconstruction.
///
/// Return `Error::InvalidShardFlags` when the number of flags does not match
/// the total number of shards.
///
/// # Variants of encoding methods
///
/// ## `sep`
///
/// Methods ending in `_sep` takes an immutable reference to data shards,
/// and a mutable reference to parity shards.
///
/// They are useful as they do not need to borrow the data shards mutably,
/// and other work that only needs read-only access to data shards can be done
/// in parallel/concurrently during the encoding.
///
/// Following is a table of all the `sep` variants
///
/// | not `sep` | `sep` |
/// | --- | --- |
/// | `encode_single_shard` | `encode_single_shard_sep` |
/// | `encode_shards` | `encode_shards_sep` |
/// | `encode_single` | `encode_single_sep` |
/// | `encode`        | `encode_sep` |
///
/// The `sep` variants do similar checks on the provided data shards and
/// parity shards.
///
/// Return `Error::TooFewDataShards`, `Error::TooManyDataShards`,
/// `Error::TooFewParityShards`, or `Error::TooManyParityShards` when applicable.
///
/// ## `single`
///
/// Methods containing `single` facilitate shard by shard encoding, where
/// the parity shards are partially constructed using one data shard at a time.
/// See `ShardByShard` struct for more details on how shard by shard encoding
/// can be useful.
///
/// They are prone to **misuse**, and it is recommended to use the `ShardByShard`
/// bookkeeping struct instead for shard by shard encoding.
///
/// The ones that are also `sep` are **ESPECIALLY** prone to **misuse**.
/// Only use them when you actually need the flexibility.
///
/// Following is a table of all the shard by shard variants
///
/// | all shards at once | shard by shard |
/// | --- | --- |
/// | `encode_shards` | `encode_single_shard` |
/// | `encode_shards_sep` | `encode_single_shard_sep` |
/// | `encode` | `encode_single` |
/// | `encode_sep` | `encode_single_sep` |
///
/// The `single` variants do similar checks on the provided data shards and parity shards,
/// and also do index check on `i_data`.
///
/// Return `Error::InvalidIndex` if `i_data >= data_shard_count`.
///
/// # Encoding behaviour
///
/// ## For `encode`, `encode_shards`
///
/// You do not need to clear the parity shards beforehand, as the methods
/// will overwrite them completely.
///
/// ## For `encode_single_shard`, `encode_single_shard_sep`, `encode_single`, `encode_single_sep`
///
/// Calling them with `i_data` being `0` will overwrite the parity shards
/// completely. If you are using the methods correctly, then you do not need
/// to clear the parity shards beforehand.
///
/// # Variants of verifying methods
///
/// `verify`, `verify_shards` allocate a buffer on the heap of the same size
/// as the parity shards, and encode the input once using the buffer to store
/// the computed parity shards, then check if the provided parity shards
/// match the computed ones.
///
/// `verify_with_buffer`, `verify_shards_with_buffer` allow you to provide
/// the buffer to avoid making heap allocation(s) for the buffer in every call.
///
/// The `with_buffer` variants also guarantee that the buffer contains the correct
/// parity shards if the result is `Ok(_)` (i.e. it does not matter whether the
/// verification passed or not, as long as the result is not an error, the buffer
/// will contain the correct parity shards after the call).
///
/// Following is a table of all the `with_buffer` variants
///
/// | not `with_buffer` | `with_buffer` |
/// | --- | --- |
/// | `verify_shards` | `verify_shards_with_buffer` |
/// | `verify` | `verify_with_buffer` |
///
/// The `with_buffer` variants also check the dimensions of the buffer and return
/// `Error::TooFewBufferShards`, `Error::TooManyBufferShards`, `Error::EmptyShard`,
/// or `Error::IncorrectShardSize` when applicable.
///
#[derive(Debug)]
pub struct ReedSolomon {
    data_shard_count   : usize,
    parity_shard_count : usize,
    total_shard_count  : usize,
    matrix             : Matrix,
    tree               : InversionTree,
    pparam             : ParallelParam
}

/// Parameters for parallelism.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct ParallelParam {
    /// Number of bytes to split the slices into for computations
    /// which can be done in parallel.
    ///
    /// Default is 32768.
    pub bytes_per_encode  : usize,
    //pub shards_per_encode : usize,
}

/// Bookkeeper for shard by shard encoding.
///
/// This is useful for avoiding incorrect use of
/// `encode_single`, `encode_single_sep`, `encode_single_shard`
/// and `encode_single_shard_sep`.
///
/// # Use cases
///
/// Shard by shard encoding is useful for streamed data encoding
/// where you do not have all the needed data shards immediately,
/// but you want to spread out the encoding workload rather than
/// doing the encoding after everything is ready.
///
/// A concrete example would be network packets encoding,
/// where encoding packet by packet as you receive them may be more efficient
/// than waiting for N packets then encode them all at once.
///
/// # Example
///
/// ```
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # use reed_solomon_erasure::*;
/// # fn main () {
/// let r = ReedSolomon::new(3, 2).unwrap();
///
/// let mut sbs = ShardByShard::new(&r);
///
/// let mut shards = shards!([0,  1,  2,  3,  4],
///                          [5,  6,  7,  8,  9],
///                          // say we don't have the 3rd data shard yet
///                          // and we want to fill it in later
///                          [0,  0,  0,  0,  0],
///                          [0,  0,  0,  0,  0],
///                          [0,  0,  0,  0,  0]);
///
/// // encode 1st and 2nd data shard
/// sbs.encode_shard(&mut shards).unwrap();
/// sbs.encode_shard(&mut shards).unwrap();
///
/// // fill in 3rd data shard
/// shards[2][0] = 10;
/// shards[2][1] = 11;
/// shards[2][2] = 12;
/// shards[2][3] = 13;
/// shards[2][4] = 14;
///
/// // now do the encoding
/// sbs.encode_shard(&mut shards).unwrap();
///
/// // above is equivalent to doing r.encode_shards(&mut shards)
/// assert!(r.verify_shards(&shards).unwrap());
/// # }
/// ```
#[derive(PartialEq, Debug)]
pub struct ShardByShard<'a> {
    codec     : &'a ReedSolomon,
    cur_input : usize,
}

impl ParallelParam {
    pub fn new(bytes_per_encode  : usize,
               /*shards_per_encode : usize*/) -> ParallelParam {
        ParallelParam { bytes_per_encode,
                        /*shards_per_encode*/ }
    }

    pub fn with_default() -> ParallelParam {
        Self::new(32768,
                  /*4*/)
    }
}

impl<'a> ShardByShard<'a> {
    /// Creates a new instance of the bookkeeping struct.
    pub fn new(codec : &'a ReedSolomon) -> ShardByShard<'a> {
        ShardByShard {
            codec,
            cur_input : 0
        }
    }

    /// Checks if the parity shards are ready to use.
    pub fn parity_ready(&self) -> bool {
        self.cur_input == self.codec.data_shard_count
    }

    /// Resets the bookkeeping data.
    ///
    /// You should call this when you have added and encoded
    /// all data shards, and have finished using the parity shards.
    ///
    /// Returns `SBSError::LeftoverShards` when there are shards encoded
    /// but parity shards are not ready to use.
    pub fn reset(&mut self) -> Result<(), SBSError>{
        if self.cur_input > 0
            && !self.parity_ready()
        {
            return Err(SBSError::LeftoverShards);
        }

        self.cur_input = 0;

        Ok(())
    }

    /// Resets the bookkeeping data without checking.
    pub fn reset_force(&mut self) {
        self.cur_input = 0;
    }

    /// Returns the current input shard index.
    pub fn cur_input_index(&self) -> usize {
        self.cur_input
    }

    fn return_ok_and_incre_cur_input(&mut self) -> Result<(), SBSError> {
        self.cur_input += 1;
        Ok(())
    }

    fn sbs_encode_shard_checks(&mut self,
                               shards : &mut [Shard])
                               -> Result<(), SBSError> {
        fn internal_checks (codec  : &ReedSolomon,
                            shards : &mut [Shard])
                            -> Result<(), Error> {
            check_piece_count!(all => codec, shards);
            check_slices!(multi => shards);

            Ok(())
        };

        if self.parity_ready() {
            return Err(SBSError::TooManyCalls);
        }

        match internal_checks(self.codec, shards) {
            Ok(()) => Ok(()),
            Err(e) => Err(SBSError::RSError(e))
        }
    }

    fn sbs_encode_checks(&mut self,
                         slices : &mut [&mut [u8]])
                         -> Result<(), SBSError> {
        fn internal_checks (codec  : &ReedSolomon,
                            slices : &mut [&mut [u8]])
                            -> Result<(), Error> {
            check_piece_count!(all => codec, slices);
            check_slices!(multi => slices);

            Ok(())
        };

        if self.parity_ready() {
            return Err(SBSError::TooManyCalls);
        }

        match internal_checks(self.codec, slices) {
            Ok(()) => Ok(()),
            Err(e) => Err(SBSError::RSError(e))
        }
    }

    fn sbs_encode_shard_sep_checks(&mut self,
                                   data   : &[Shard],
                                   parity : &mut [Shard])
                                   -> Result<(), SBSError> {
        fn internal_checks (codec  : &ReedSolomon,
                            data   : &[Shard],
                            parity : &mut [Shard])
                            -> Result<(), Error> {
            check_piece_count!(data   => codec, data);
            check_piece_count!(parity => codec, parity);
            check_slices!(multi => data, multi => parity);

            Ok(())
        };

        if self.parity_ready() {
            return Err(SBSError::TooManyCalls);
        }

        match internal_checks(self.codec, data, parity) {
            Ok(()) => Ok(()),
            Err(e) => Err(SBSError::RSError(e))
        }
    }

    fn sbs_encode_sep_checks(&mut self,
                             data   : &[&[u8]],
                             parity : &mut [&mut [u8]])
                             -> Result<(), SBSError> {
        fn internal_checks (codec  : &ReedSolomon,
                            data   : &[&[u8]],
                            parity : &mut [&mut [u8]])
                            -> Result<(), Error> {
            check_piece_count!(data   => codec, data);
            check_piece_count!(parity => codec, parity);
            check_slices!(multi => data, multi => parity);

            Ok(())
        };

        if self.parity_ready() {
            return Err(SBSError::TooManyCalls);
        }

        match internal_checks(self.codec, data, parity) {
            Ok(()) => Ok(()),
            Err(e) => Err(SBSError::RSError(e))
        }
    }

    /// Constructs the parity shards partially using the current input data shard.
    ///
    /// Returns `SBSError::TooManyCalls` when all input data shards
    /// have already been filled in via `encode` or `encode_shard`.
    pub fn encode(&mut self,
                  slices : &mut [&mut [u8]])
                  -> Result<(), SBSError> {
        self.sbs_encode_checks(slices)?;

        self.codec.encode_single(self.cur_input,
                                 slices).unwrap();

        self.return_ok_and_incre_cur_input()
    }

    /// Constructs the parity shards partially using the current input data shard.
    ///
    /// Returns `SBSError::TooManyCalls` when all input data shards
    /// have already been filled in via `encode` or `encode_shard`.
    pub fn encode_sep(&mut self,
                      data   : &[&[u8]],
                      parity : &mut [&mut[u8]])
                      -> Result<(), SBSError> {
        self.sbs_encode_sep_checks(data, parity)?;

        self.codec.encode_single_sep(self.cur_input,
                                     &data[self.cur_input],
                                     parity).unwrap();

        self.return_ok_and_incre_cur_input()
    }

    /// Constructs the parity shards partially using the current input data shard.
    ///
    /// Returns `SBSError::TooManyCalls` when all input data shards
    /// have already been filled in via `encode` or `encode_shard`.
    pub fn encode_shard(&mut self,
                        shards : &mut [Shard])
                        -> Result<(), SBSError> {
        self.sbs_encode_shard_checks(shards)?;

        self.codec.encode_single_shard(self.cur_input,
                                       shards).unwrap();

        self.return_ok_and_incre_cur_input()
    }

    /// Constructs the parity shards partially using the current input data shard.
    ///
    /// Returns `TooManyCalls` when all input data shards
    /// have already been filled in via `encode` or `encode_shard`.
    pub fn encode_shard_sep(&mut self,
                            data   : &[Shard],
                            parity : &mut [Shard])
                            -> Result<(), SBSError> {
        self.sbs_encode_shard_sep_checks(data, parity)?;

        self.codec.encode_single_shard_sep(self.cur_input,
                                           &data[self.cur_input],
                                           parity).unwrap();

        self.return_ok_and_incre_cur_input()
    }
}

impl Clone for ReedSolomon {
    fn clone(&self) -> ReedSolomon {
        ReedSolomon::with_pparam(self.data_shard_count,
                                 self.parity_shard_count,
                                 self.pparam.clone()).unwrap()
    }
}

impl PartialEq for ReedSolomon {
    fn eq(&self, rhs : &ReedSolomon) -> bool {
        self.data_shard_count == rhs.data_shard_count
            && self.parity_shard_count == rhs.parity_shard_count
    }
}

impl ReedSolomon {
    // AUDIT
    //
    // Error detection responsibilities
    //
    // Terminologies and symbols :
    //   X =A, B, C=> Y : X delegates error checking responsibilities A, B, C to Y
    //   X := A, B, C   : X needs to handle responsibilities A, B, C
    //
    // Encode methods
    //
    // `encode_single_shard`     =ALL=> `encode_single`
    // `encode_single_shard_sep` =ALL=> `encode_single_sep`
    // `encode_shards`           =ALL=> `encode`
    // `encode_shards_sep`       =ALL=> `encode_sep`
    // `encode_single` :=
    //   - check index `i_data` within range [0, data shard count)
    //   - check length of `slices` matches total shard count exactly
    //   - check consistency of length of individual slices
    // `encode_single_sep` :=
    //   - check index `i_data` within range [0, data shard count)
    //   - check length of `parity` matches parity shard count exactly
    //   - check consistency of length of individual parity slices
    //   - check length of `single_data` matches length of first parity slice
    // `encode` :=
    //   - check length of `slices` matches total shard count exactly
    //   - check consistency of length of individual slices
    // `encode_sep` :=
    //   - check length of `data` matches data shard count exactly
    //   - check length of `parity` matches parity shard count exactly
    //   - check consistency of length of individual data slices
    //   - check consistency of length of individual parity slices
    //   - check length of first parity slice matches length of first data slice
    //
    // Verify methods
    //
    // `verify_shards`             =ALL=> `verify_shards_with_buffer`
    // `verify_shards_with_buffer` =ALL=> `verify_with_buffer`
    // `verify` :=
    //   - check length of `slices` matches total shard count exactly
    //   - check consistency of length of individual slices
    //
    //   Generates buffer then passes control to verify_with_buffer
    //
    // `verify_with_buffer` :=
    //   - check length of `slices` matches total shard count exactly
    //   - check length of `buffer` matches parity shard count exactly
    //   - check consistency of length of individual slices
    //   - check consistency of length of individual slices in buffer
    //   - check length of first slice in buffer matches length of first slice
    //
    // Reconstruct methods
    //
    // `reconstruct`             =ALL=> `reconstruct_internal`
    // `reconstruct_data`        =ALL=> `reconstruct_internal`
    // `reconstruct_shards`      =ALL=> `reconstruct_shards_internal`
    // `reconstruct_data_shards` =ALL=> `reconstruct_shards_internal`
    // `reconstruct_shards_internal` :=
    //   - check length of `shards` matches total shard count exactly
    //   - check at least one option shard is not `None`
    //   - check consistency of length of individual option shards if exist
    // `reconstruct_internal` :=
    //   - check length of `slices` matches total shard count exactly
    //   - check consistency of length of individual slices
    //   - check length of `slice_present` matches length of `slices`

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
    ///
    /// Returns `Error::TooFewDataShards` if `data_shards == 0`.
    ///
    /// Returns `Error::TooFewParityShards` if `parity_shards == 0`.
    ///
    /// Returns `Error::TooManyShards` if `data_shards + parity_shards > 256`.
    pub fn new(data_shards : usize,
               parity_shards : usize)
               -> Result<ReedSolomon, Error> {
        Self::with_pparam(data_shards,
                          parity_shards,
                          ParallelParam::with_default())
    }

    /// Creates a new instance of Reed-Solomon erasure code encoder/decoder with custom `ParallelParam`.
    ///
    /// If `pparam.bytes_per_encode == 0`, it will be set to `1`.
    ///
    /// Returns `Error::TooFewDataShards` if `data_shards == 0`.
    ///
    /// Returns `Error::TooFewParityShards` if `parity_shards == 0`.
    ///
    /// Returns `Error::TooManyShards` if `data_shards + parity_shards > 256`.
    pub fn with_pparam(data_shards   : usize,
                       parity_shards : usize,
                       mut pparam    : ParallelParam)
                       -> Result<ReedSolomon, Error> {
        if data_shards == 0 {
            return Err(Error::TooFewDataShards);
        }
        if parity_shards == 0 {
            return Err(Error::TooFewParityShards);
        }
        if data_shards + parity_shards > 256 {
            return Err(Error::TooManyShards);
        }

        let total_shards = data_shards + parity_shards;

        let matrix       = Self::build_matrix(data_shards, total_shards);

        if pparam.bytes_per_encode == 0 {
            pparam.bytes_per_encode = 1;
        }

        Ok(ReedSolomon {
            data_shard_count   : data_shards,
            parity_shard_count : parity_shards,
            total_shard_count  : total_shards,
            matrix,
            tree               : InversionTree::new(data_shards, parity_shards),
            pparam
        })
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
        for i_input in 0..self.data_shard_count {
            self.code_single_slice(matrix_rows,
                                   i_input,
                                   inputs[i_input],
                                   outputs);
        }
    }

    fn code_single_slice(&self,
                         matrix_rows : &[&[u8]],
                         i_input     : usize,
                         input       : &[u8],
                         outputs     : &mut [&mut [u8]]) {
        outputs
            .into_par_iter()
            .enumerate()
            .for_each(|(i_row, output)| {
                let matrix_row_to_use = matrix_rows[i_row][i_input];

                if i_input == 0 {
                    if output.len() <= self.pparam.bytes_per_encode {
                        galois::mul_slice(matrix_row_to_use,
                                          input,
                                          output);
                    } else {
                        output.par_chunks_mut(self.pparam.bytes_per_encode)
                            .into_par_iter()
                            .enumerate()
                            .for_each(|(i, output)| {
                                let start =
                                    i * self.pparam.bytes_per_encode;
                                galois::mul_slice(matrix_row_to_use,
                                                  &input[start..start + output.len()],
                                                  output);
                            })
                    }
                } else {
                    if output.len() <= self.pparam.bytes_per_encode {
                        galois::mul_slice_xor(matrix_row_to_use,
                                              input,
                                              output);
                    } else {
                        output.par_chunks_mut(self.pparam.bytes_per_encode)
                            .into_par_iter()
                            .enumerate()
                            .for_each(|(i, output)| {
                                let start =
                                    i * self.pparam.bytes_per_encode;
                                galois::mul_slice_xor(matrix_row_to_use,
                                                      &input[start..start + output.len()],
                                                      output);
                            })
                    }
                }
            })
    }

    fn check_some_slices_with_buffer(&self,
                                     matrix_rows  : &[&[u8]],
                                     inputs       : &[&[u8]],
                                     to_check     : &[&[u8]],
                                     buffer       : &mut [&mut [u8]])
                                     -> bool {
        self.code_some_slices(matrix_rows,
                              inputs,
                              buffer);

        // AUDIT
        //
        // `misc_utils::par_slices_are_equal` uses the same short-circuiting logic
        // as the following code
        //
        // The logic is detailed in the AUDIT notes in that function

        let at_least_one_mismatch_present =
            buffer
            .par_iter_mut()
            .enumerate()
            .map(|(i, expected_parity_shard)| {
                misc_utils::par_slices_are_equal(expected_parity_shard,
                                                 to_check[i],
                                                 self.pparam.bytes_per_encode)
            })
            .any(|x| !x);  // find the first false(some slice is different from the expected one)
        !at_least_one_mismatch_present
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

    /// Constructs the parity shards partially using only the data shard
    /// indexed by `i_data`.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    /// This is a wrapper of `encode_single`.
    ///
    /// # Warning
    ///
    /// You must apply this method on the data shards in strict sequential order(0..data shard count),
    /// otherwise the parity shards will be incorrect.
    ///
    /// It is recommended to use the `ShardByShard` bookkeeping struct instead of this method directly.
    pub fn encode_single_shard(&self,
                               i_data : usize,
                               shards : &mut [Shard]) -> Result<(), Error> {
        let mut slices : SmallVec<[&mut [u8]; 32]> =
            convert_2D_slices!(shards =>into SmallVec<[&mut [u8]; 32]>,
                               SmallVec::with_capacity);

        self.encode_single(i_data, &mut slices)
    }

    /// Constructs the parity shards partially using only the data shard provided.
    ///
    /// The data shard must match the index `i_data`.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    /// This is a wrapper of `encode_single_sep`.
    ///
    /// # Warning
    ///
    /// You must apply this method on the data shards in strict sequential order(0..data shard count),
    /// otherwise the parity shards will be incorrect.
    ///
    /// It is recommended to use the `ShardByShard` bookkeeping struct instead of this method directly.
    pub fn encode_single_shard_sep(&self,
                                   i_data      : usize,
                                   single_data : &Shard,
                                   parity      : &mut[Shard]) -> Result<(), Error> {
        let mut parity : SmallVec<[&mut [u8]; 32]> =
            convert_2D_slices!(parity =>into SmallVec<[&mut [u8]; 32]>,
                               SmallVec::with_capacity);

        self.encode_single_sep(i_data, single_data, &mut parity)
    }

    /// Constructs the parity shards.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    /// This is a wrapper of `encode`.
    pub fn encode_shards(&self,
                         shards : &mut [Shard]) -> Result<(), Error> {
        let mut slices : SmallVec<[&mut [u8]; 32]> =
            convert_2D_slices!(shards =>into SmallVec<[&mut [u8]; 32]>,
                               SmallVec::with_capacity);

        self.encode(&mut slices)
    }

    /// Constructs the parity shards.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    /// This is a wrapper of `encode_sep`.
    pub fn encode_shards_sep(&self,
                             data   : &[Shard],
                             parity : &mut [Shard]) -> Result<(), Error> {
        let     data   : SmallVec<[&[u8]; 32]> =
            convert_2D_slices!(data   =>into SmallVec<[&[u8]; 32]>,
                               SmallVec::with_capacity);

        let mut parity : SmallVec<[&mut [u8]; 32]> =
            convert_2D_slices!(parity =>into SmallVec<[&mut [u8]; 32]>,
                               SmallVec::with_capacity);

        self.encode_sep(&data, &mut parity)
    }

    /// Constructs the parity shards partially using only the data shard
    /// indexed by `i_data`.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    /// # Warning
    ///
    /// You must apply this method on the data shards in strict sequential order(0..data shard count),
    /// otherwise the parity shards will be incorrect.
    ///
    /// It is recommended to use the `ShardByShard` bookkeeping struct instead of this method directly.
    pub fn encode_single(&self,
                         i_data  : usize,
                         slices  : &mut [&mut [u8]]) -> Result<(), Error> {
        check_slice_index!(data => self, i_data);
        check_piece_count!(all  => self, slices);
        check_slices!(multi => slices);

        // Get the slice of output buffers.
        let (mut_input, output) =
            slices.split_at_mut(self.data_shard_count);

        let input = &mut_input[i_data];

        self.encode_single_sep(i_data, input, output)
    }

    /// Constructs the parity shards partially using only the data shard provided.
    ///
    /// The data shard must match the index `i_data`.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    /// # Warning
    ///
    /// You must apply this method on the data shards in strict sequential order(0..data shard count),
    /// otherwise the parity shards will be incorrect.
    ///
    /// It is recommended to use the `ShardByShard` bookkeeping struct instead of this method directly.
    pub fn encode_single_sep(&self,
                             i_data      : usize,
                             single_data : &[u8],
                             parity      : &mut[&mut[u8]]) -> Result<(), Error> {
        check_slice_index!(data   => self, i_data);
        check_piece_count!(parity => self, parity);
        check_slices!(multi => parity, single => single_data);

        let parity_rows = self.get_parity_rows();

        // Do the coding.
        self.code_single_slice(&parity_rows,
                               i_data,
                               single_data,
                               parity);

        Ok(())
    }

    /// Constructs the parity shards.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    pub fn encode(&self,
                  slices : &mut [&mut [u8]]) -> Result<(), Error> {
        check_piece_count!(all => self, slices);
        check_slices!(multi => slices);

        // Get the slice of output buffers.
        let (mut_input, output) =
            slices.split_at_mut(self.data_shard_count);

        let input =
            convert_2D_slices!(mut_input =>into SmallVec<[&[u8]; 32]>,
                               SmallVec::with_capacity);

        self.encode_sep(&input, output)
    }

    /// Constructs the parity shards.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    pub fn encode_sep(&self,
                      data   : &[&[u8]],
                      parity : &mut [&mut[u8]]) -> Result<(), Error> {
        check_piece_count!(data   => self, data);
        check_piece_count!(parity => self, parity);
        check_slices!(multi => data, multi => parity);

        let parity_rows = self.get_parity_rows();

        // Do the coding.
        self.code_some_slices(&parity_rows,
                              data,
                              parity);

        Ok(())
    }

    /// Checks if the parity shards are correct.
    ///
    /// This is a wrapper of `verify_shards_with_buffer`.
    pub fn verify_shards(&self,
                         shards : &[Shard]) -> Result<bool, Error> {
        let mut buffer : SmallVec<[Box<[u8]>; 32]> =
            SmallVec::with_capacity(self.parity_shard_count);
        for _ in 0..self.parity_shard_count {
            buffer.push(vec![0; shards[0].len()].into_boxed_slice());
        }

        self.verify_shards_with_buffer(shards, &mut buffer)
    }

    /// Checks if the parity shards are correct.
    ///
    /// This is a wrapper of `verify_with_buffer`.
    pub fn verify_shards_with_buffer(&self,
                                     shards : &[Shard],
                                     buffer : &mut [Shard])
                                     -> Result<bool, Error> {
        let slices =
            convert_2D_slices!(shards =>to SmallVec<[&[u8]; 32]>,
                               SmallVec::with_capacity);

        let mut buffer =
            convert_2D_slices!(buffer =>to_mut SmallVec<[&mut [u8]; 32]>,
                               SmallVec::with_capacity);

        self.verify_with_buffer(&slices, &mut buffer)
    }

    /// Checks if the parity shards are correct.
    ///
    /// This is a wrapper of `verify_with_buffer`.
    pub fn verify(&self,
                  slices : &[&[u8]]) -> Result<bool, Error> {
        check_piece_count!(all => self, slices);
        check_slices!(multi => slices);

        let mut buffer : SmallVec<[Vec<u8>; 32]> =
            SmallVec::with_capacity(self.parity_shard_count);
        for _ in 0..self.parity_shard_count {
            buffer.push(vec![0; slices[0].len()]);
        }

        let mut buffer =
            convert_2D_slices!(buffer =>to_mut SmallVec<[&mut [u8]; 32]>,
                               SmallVec::with_capacity);

        self.verify_with_buffer(slices, &mut buffer)
    }

    /// Checks if the parity shards are correct.
    pub fn verify_with_buffer(&self,
                              slices : &[&[u8]],
                              buffer : &mut [&mut [u8]])
                              -> Result<bool, Error> {
        check_piece_count!(all        => self, slices);
        check_piece_count!(parity_buf => self, buffer);
        check_slices!(multi => slices, multi => buffer);

        let data        = &slices[0..self.data_shard_count];

        let to_check    = &slices[self.data_shard_count..];

        let parity_rows = self.get_parity_rows();

        Ok(self.check_some_slices_with_buffer(&parity_rows,
                                              data,
                                              to_check,
                                              buffer))
    }

    /// Reconstructs all shards.
    ///
    /// The shards marked not present are only overwritten when no error
    /// is detected.
    ///
    /// This means if the method returns an `Error`, then nothing is touched.
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
    /// The shards marked not present are only overwritten when no error
    /// is detected.
    ///
    /// This means if the method returns an `Error`, then nothing is touched.
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
    /// This fills in the missing shards with blank shards if and only
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
    /// This fills in the missing data shards with blank shards if and only
    /// if there are enough shards for reconstruction.
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
        check_piece_count!(all => self, shards);

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

        {
            let mut slices =
                mut_option_shards_to_mut_slices(shards);

            // AUDIT
            //
            // The above checks cover all the checks done in
            // `reconstruct_internal` already, so calling
            // unwrap should be completely safe

            self.reconstruct_internal(&mut slices,
                                      &shard_present,
                                      data_only).unwrap();
        }

        if data_only {
            // Remove filled in parity shards
            for i in self.data_shard_count..self.total_shard_count {
                if !shard_present[i] {
                    shards[i] = None;
                }
            }
        }

        Ok(())
    }

    fn get_data_decode_matrix(&self,
                              valid_indices   : &[usize],
                              invalid_indices : &[usize])
                              -> Arc<Matrix> {
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
                for (sub_matrix_row, &valid_index) in
                    valid_indices.into_iter().enumerate()
                {
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
                self.tree.insert_inverted_matrix(&invalid_indices,
                                                 &data_decode_matrix).unwrap();

                data_decode_matrix
            },
            Some(m) => {
                m
            }
        }
    }

    fn reconstruct_internal(&self,
                            slices        : &mut [&mut [u8]],
                            slice_present : &[bool],
                            data_only     : bool) -> Result<(), Error> {
        check_piece_count!(all => self, slices);
        check_slices!(multi => slices);

        if slices.len() != slice_present.len() {
            return Err(Error::InvalidShardFlags);
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
        // and the invalid rows we don't have.
        //
        // The valid indices are used to construct the data decode matrix,
        // the invalid indices are used to key the data decode matrix
        // in the inversion tree.
        //
        // We only need exactly N valid indices, where N = `data_shard_count`,
        // as the data decode matrix is a N x N matrix, thus only needs
        // N valid indices for determining the N rows to pick from
        // `self.matrix`.
        let mut sub_shards             : SmallVec<[&[u8];     32]> =
            SmallVec::with_capacity(self.data_shard_count);
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
                    // Already have enough shards in `sub_shards`
                    // as we only need N shards, where N = `data_shard_count`,
                    // for the data decode matrix
                    //
                    // So nothing to do here
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
                                        &invalid_indices);

        // Re-create any data shards that were missing.
        //
        // The input to the coding is all of the shards we actually
        // have, and the output is the missing data shards. The computation
        // is done using the special decode matrix we just built.
        let mut matrix_rows : SmallVec<[&[u8]; 32]> =
            SmallVec::with_capacity(self.parity_shard_count);
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
                // Gather up all the data slices
                let mut i_old_data_slice = 0;
                let mut i_new_data_slice = 0;
                let mut all_data_slices : SmallVec<[&[u8]; 32]> =
                    SmallVec::with_capacity(self.data_shard_count);
                for i_slice in 0..self.data_shard_count {
                    let slice =
                        if slice_present[i_slice] {
                            let result : &[u8] =
                                sub_shards[i_old_data_slice];
                            i_old_data_slice += 1;
                            result
                        } else {
                            let result : &[u8] =
                                missing_data_slices[i_new_data_slice];
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
