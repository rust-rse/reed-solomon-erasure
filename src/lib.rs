//! This crate providers an encoder/decoder for Reed-Solomon erasure code
//!
//! Please note that erasure coding means errors are not directly detected or corrected,
//! but missing data pieces(shards) can be reconstructed given that
//! the configuration provides high enough redundancy.
//!
//! You will have to implement error detection separately(e.g. via checksums)
//! and simply leave out the corrupted shards when attempting to reconstruct
//! the missing data.

#![allow(dead_code)]
mod misc_utils;
mod galois;
mod matrix;
mod inversion_tree;

extern crate rayon;
use rayon::prelude::*;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

extern crate num_cpus;

use matrix::Matrix;
use inversion_tree::InversionTree;

#[derive(PartialEq, Debug)]
pub enum Error {
    TooFewShards,
    WrongShardSize,
    EmptyShard,
    InvalidShardsIndicator,
    InversionTreeError(inversion_tree::Error)
}

/// Main data type used by this library
pub type Shard = Box<[u8]>;

/// Constructs a shard
///
/// # Example
/// ```rust
/// # #[macro_use] extern crate reed_solomon_erasure;
/// # use reed_solomon_erasure::*;
/// # fn main () {
/// let shard = shard!(1, 2, 3);
/// # }
/// ```
#[macro_export]
macro_rules! shard {
    (
        $( $x:expr ),*
    ) => {
        Box::new([ $( $x ),* ])
    }
}

/// Constructs vector of shards
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
        vec![ $( Box::new([ $( $x ),* ])),* ]
    }}
}

mod helper {
    use super::*;

    pub fn calc_offset(offset : Option<usize>) -> usize {
        match offset {
            Some(x) => x,
            None    => 0
        }
    }

    pub fn calc_byte_count(shards     : &Vec<Shard>,
                           byte_count : Option<usize>) -> usize {
        let result = match byte_count {
            Some(x) => x,
            None    => shards[0].len()
        };

        if result == 0 { panic!("Byte count is zero"); }

        result
    }

    pub fn calc_offset_and_byte_count(offset : Option<usize>,
                                      shards : &Vec<Shard>,
                                      byte_count : Option<usize>)
                                      -> (usize, usize) {
        let offset     = calc_offset(offset);
        let byte_count = calc_byte_count(shards, byte_count);

        (offset, byte_count)
    }

    pub fn calc_byte_count_option_shards(shards     : &Vec<Option<Shard>>,
                                         byte_count : Option<usize>) -> usize {
        let result = match byte_count {
            Some(x) => x,
            None    => {
                let mut value = None;
                for v in shards.iter() {
                    match *v {
                        Some(ref x) => { value = Some(x.len());
                                         break; },
                        None        => {},
                    }
                };
                match value {
                    Some(v) => v,
                    None    => panic!("No shards are present")
                }
            }
        };

        if result == 0 { panic!("Byte count is zero"); }

        result
    }

    pub fn calc_offset_and_byte_count_option_shards(offset : Option<usize>,
                                                    shards : &Vec<Option<Shard>>,
                                                    byte_count : Option<usize>)
                                                    -> (usize, usize) {
        let offset     = calc_offset(offset);
        let byte_count = calc_byte_count_option_shards(shards, byte_count);

        (offset, byte_count)
    }

}

/// Makes shard with byte array of zero length
pub fn make_zero_len_shard() -> Shard {
    Box::new([])
}

pub fn make_zero_len_shards(count : usize) -> Vec<Shard> {
    let mut result = Vec::with_capacity(count);
    for _ in 0..count {
        result.push(make_zero_len_shard());
    }
    result
}

/// Makes shard with byte array filled with zeros of some length
pub fn make_blank_shard(size : usize) -> Shard {
    vec![0; size].into_boxed_slice()
}

pub fn make_blank_shards(size : usize, count : usize) -> Vec<Shard> {
    let mut result = Vec::with_capacity(count);
    for _ in 0..count {
        result.push(make_blank_shard(size));
    }
    result
}

/// Transforms vector of shards to vector of option shards
///
/// # Remarks
///
/// Each shard is cloned rather than moved, which may be slow.
///
/// This is mainly useful when you want to repair a vector
/// of shards using `decode_missing`.
pub fn shards_to_option_shards(shards : &Vec<Shard>)
                               -> Vec<Option<Shard>> {
    let mut result = Vec::with_capacity(shards.len());

    for v in shards.iter() {
        let inner : Box<[u8]> = v.clone();
        result.push(Some(inner));
    }
    result
}

/// Transforms vector of shards into vector of option shards
///
/// # Remarks
///
/// Each shard is moved rather than cloned.
///
/// This is mainly useful when you want to repair a vector
/// of shards using `decode_missing`.
pub fn shards_into_option_shards(shards : Vec<Shard>)
                                 -> Vec<Option<Shard>> {
    let mut result = Vec::with_capacity(shards.len());

    for v in shards.into_iter() {
        result.push(Some(v));
    }
    result
}

/// Transforms a section of vector of option shards to vector of shards
///
/// # Arguments
///
/// * `start` - start of range of option shards you want to use
/// * `count` - number of option shards you want to use
///
/// # Remarks
///
/// Each shard is cloned rather than moved, which may be slow.
///
/// This is mainly useful when you want to convert result of
/// `decode_missing` to the more usable arrangement.
///
/// Panics when any of the shards is missing or the range exceeds number of shards provided.
pub fn option_shards_to_shards(shards : &Vec<Option<Shard>>,
                               start  : Option<usize>,
                               count  : Option<usize>)
                               -> Vec<Shard> {
    let offset = helper::calc_offset(start);
    let count  = match count {
        None    => shards.len(),
        Some(x) => x
    };

    if shards.len() < offset + count {
        panic!("Too few shards, number of shards : {}, offset + count : {}", shards.len(), offset + count);
    }

    let mut result = Vec::with_capacity(shards.len());

    for i in offset..offset + count {
        let shard = match shards[i] {
            Some(ref x) => x,
            None        => panic!("Missing shard, index : {}", i),
        };
        let inner : Box<[u8]> = shard.clone();
        result.push(inner);
    }
    result
}

/// Transforms vector of option shards into vector of shards
///
/// # Remarks
///
/// Each shard is moved rather than cloned.
///
/// This is mainly useful when you want to convert result of
/// `decode_missing` to the more usable arrangement.
///
/// Panics when any of the shards is missing.
pub fn option_shards_into_shards(shards : Vec<Option<Shard>>)
                                 -> Vec<Shard> {
    let mut result = Vec::with_capacity(shards.len());

    for shard in shards.into_iter() {
        let shard = match shard {
            Some(x) => x,
            None    => panic!("Missing shard"),
        };
        result.push(shard);
    }
    result
}

/// Deep copies vector of shards
///
/// # Remarks
///
/// Normally doing `shards.clone()` (where `shards` is a `Vec<Shard>`) is okay,
/// but the `Rc` in `Shard`'s definition will cause it to be a shallow copy, rather
/// than a deep copy.
///
/// If the shards are used immutably, then a shallow copy is more desirable, as it
/// has significantly lower overhead.
///
/// If the shards are used mutably, then a deep copy may be more desirable, as this
/// will avoid unexpected bugs caused by multiple ownership.
pub fn deep_clone_shards(shards : &Vec<Shard>) -> Vec<Shard> {
    let mut result = Vec::with_capacity(shards.len());

    for v in shards.iter() {
        let inner : Box<[u8]> = v.clone();
        result.push(inner);
    }
    result
}

/// Deep copies vector of option shards
///
/// # Remarks
///
/// Normally doing `shards.clone()` (where `shards` is a `Vec<Option<Shard>>`) is okay,
/// but the `Rc` in `Shard`'s definition will cause it to be a shallow copy, rather
/// than a deep copy.
///
/// If the shards are used immutably, then a shallow copy is more desirable, as it
/// has significantly lower overhead.
///
/// If the shards are used mutably, then a deep copy may be more desirable, as this
/// will avoid unexpected bugs caused by multiple ownership.
pub fn deep_clone_option_shards(shards : &Vec<Option<Shard>>) -> Vec<Option<Shard>> {
    let mut result = Vec::with_capacity(shards.len());

    for v in shards.iter() {
        let inner = match *v {
            Some(ref x) => { let inner = x.clone();
                             Some(inner) },
            None        => None
        };
        result.push(inner);
    }
    result
}

fn shards_to_refs<'a>(shards : &'a Vec<Shard>) -> Vec<&'a [u8]> {
    let mut result = Vec::with_capacity(shards.len());
    for shard in shards.iter() {
        result.push(shard.deref());
    }
    result
}

fn mut_shards_to_mut_refs<'a>(shards : &'a mut Vec<Shard>)
                              -> Vec<&'a mut [u8]> {
    let mut result = Vec::with_capacity(shards.len());
    for shard in shards.iter_mut() {
        result.push(shard.deref_mut());
    }
    result
}

/// Reed-Solomon erasure code encoder/decoder
///
/// # Remarks
/// Notes about usage of `offset` and `byte_count` for all methods/functions below
///
/// `offset` refers to start of the shard you want to as starting point for encoding/decoding.
///
/// `offset` defaults to 0 if it is `None`.
///
///  `byte_count` refers to number of bytes, starting from `offset` to use for encoding/decoding.
///
///  `byte_count` defaults to length of shard if it is `None`.
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

/// Parameters for parallelism
#[derive(PartialEq, Debug, Clone)]
pub struct ParallelParam {
    pub bytes_per_encode  : usize,
    pub shards_per_encode : usize,
}

impl ParallelParam {
    pub fn new(bytes_per_encode  : usize,
               shards_per_encode : usize) -> ParallelParam {
        ParallelParam { bytes_per_encode,
                        shards_per_encode }
    }

    pub fn with_default() -> ParallelParam {
        Self::new(4096,
                  4)
    }
}

impl Clone for ReedSolomon {
    fn clone(&self) -> ReedSolomon {
        ReedSolomon::with_pparam(self.data_shard_count,
                                 self.parity_shard_count,
                                 self.pparam.clone())
    }
}

impl ReedSolomon {
    fn get_parity_rows(&self) -> Vec<&[u8]> {
        let mut parity_rows  = Vec::with_capacity(self.parity_shard_count);
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

    pub fn new(data_shards : usize,
               parity_shards : usize) -> ReedSolomon {
        Self::with_pparam(data_shards,
                          parity_shards,
                          ParallelParam::with_default())
    }

    /// Creates a new instance of Reed-Solomon erasure code encoder/decoder
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

    fn breakdown_mut_option_shards(shards : &mut [Option<Shard>])
                                    -> Vec<&mut Option<Shard>> {
        misc_utils::breakdown_slice_mut(shards)
    }

    fn breakdown_mut_option_shards_to_refs<'a>(shards : &'a [&mut Option<Shard>])
                                            -> Vec<&'a [u8]> {
        let tmp = misc_utils::breakdown_slice(shards);
        let mut result : Vec<&[u8]> =
            Vec::with_capacity(tmp.len());
        for s in tmp.into_iter() {
            match **s {
                None        => panic!("Slot is empty"),
                Some(ref s) => result.push(s)
            }
        }
        result
    }

    fn breakdown_mut_shards(shards : &mut [Shard])
                             -> Vec<&mut [u8]> {
        let tmp = misc_utils::breakdown_slice_mut(shards);
        let mut result : Vec<&mut [u8]> =
            Vec::with_capacity(tmp.len());
        for s in tmp.into_iter() {
            result.push(s)
        }
        result
    }

    fn breakdown_shards_ref<'a>(shards : &'a [&Shard])
                                 -> Vec<&'a [u8]> {
        let tmp = misc_utils::breakdown_slice(shards);
        let mut result : Vec<&'a [u8]> =
            Vec::with_capacity(tmp.len());
        for s in tmp.into_iter() {
            result.push(s)
        }
        result
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

    /*fn code_some_option_shards(&self,
                               matrix_rows  : &[&[u8]],
                               inputs       : &[&[u8]],
                               outputs      : &mut [&mut Option<Shard>],
                               output_count : usize) {
        self.code_some_slices(matrix_rows,
                              inputs,
                              &mut Self::mut_option_shards_to_mut_slices(
                                  outputs))
    }*/

    fn code_some_slices(&self,
                        matrix_rows  : &[&[u8]],
                        inputs       : &[&[u8]],
                        outputs      : &mut [&mut [u8]]) {
        for c in 0..self.data_shard_count {
            let input = inputs[c];
            misc_utils::breakdown_slice_mut_with_index(
                outputs)
                .into_par_iter()
                .for_each(|(i_row, output)| {
                    if c == 0 {
                        misc_utils::split_slice_mut
                            (output, self.pparam.bytes_per_encode)
                            .into_par_iter()
                            .for_each(|output| {
                                galois::mul_slice(matrix_rows[i_row][c],
                                                  input,
                                                  output);
                            })
                    } else {
                        misc_utils::split_slice_mut
                            (output, self.pparam.bytes_per_encode)
                            .into_par_iter()
                            .for_each(|output| {
                                galois::mul_slice_xor(matrix_rows[i_row][c],
                                                      input,
                                                      output);
                            })
                    }
                })
        }
    }

    fn check_some_shards(&self,
                         matrix_rows  : &[&[u8]],
                         inputs       : &[&[u8]],
                         to_check     : &[&[u8]],
                         output_count : usize)
                         -> bool {
        let mut outputs =
            make_blank_shards(inputs[0].len(), to_check.len());
        for c in 0..self.data_shard_count {
            let input = inputs[c];
            misc_utils::breakdown_slice_mut_with_index
                (&mut outputs[0..output_count])
                .into_par_iter()
                .for_each(|(i_row, output)| {
                    galois::mul_slice_xor(matrix_rows[i_row][c],
                                          input,
                                          output);
                })
        }
        for i in 0..outputs.len() {
            if !misc_utils::slices_are_equal(&outputs[i], to_check[i]) {
                return false;
            }
        }
        true
    }

    fn check_slices(slices : &[&[u8]]) -> Result<(), Error> {
        let size = slices[0].len();
        if size == 0 {
            return Err(Error::EmptyShard);
        }
        for slice in slices.iter() {
            if slice.len() != size {
                return Err(Error::WrongShardSize);
            }
        }
        Ok(())
    }

    fn check_mut_slices(slices : &[&mut [u8]]) -> Result<(), Error> {
        let size = slices[0].len();
        if size == 0 {
            return Err(Error::EmptyShard);
        }
        for slice in slices.iter() {
            if slice.len() != size {
                return Err(Error::WrongShardSize);
            }
        }
        Ok(())
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
            None    => return Err(Error::EmptyShard),
            Some(size) => {
                for slice in slices.iter() {
                    match *slice {
                        None => {},
                        Some(ref slice) => {
                            if slice.len() != size {
                                return Err(Error::WrongShardSize);
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn encode(&self,
                  slices : &mut [&mut [u8]]) -> Result<(), Error> {
        if slices.len() < self.total_shard_count {
            return Err(Error::TooFewShards);
        }

        Self::check_mut_slices(slices)?;

        let parity_rows = self.get_parity_rows();

	      // Get the slice of output buffers.
        let (mut_input, output) =
            slices.split_at_mut(self.data_shard_count);

        let mut input : Vec<&[u8]> =
            Vec::with_capacity(mut_input.len());
        for i in mut_input.into_iter() {
            input.push(i);
        }

	      // Do the coding.
        self.code_some_slices(&parity_rows,
                              &input,
                              output);

        Ok(())
    }

    pub fn reconstruct(&self,
                       slices        : &mut [&mut [u8]],
                       slice_present : &[bool]) -> Result<(), Error> {
        self.reconstruct_internal(slices,
                                  slice_present,
                                  false)
    }

    pub fn reconstruct_data(&self,
                            slices        : &mut [&mut [u8]],
                            slice_present : &[bool]) -> Result<(), Error> {
        self.reconstruct_internal(slices,
                                  slice_present,
                                  true)
    }

    pub fn reconstruct_shards(&self,
                              shards : &mut [Option<Shard>]) -> Result<(), Error> {
        self.reconstruct_shards_internal(shards, false)
    }

    pub fn reconstruct_data_shards(&self,
                                   shards : &mut [Option<Shard>]) -> Result<(), Error> {
        self.reconstruct_shards_internal(shards, true)
    }

    fn reconstruct_shards_internal(&self,
                                   shards    : &mut [Option<Shard>],
                                   data_only : bool)
                                   -> Result<(), Error> {
        if shards.len() < self.total_shard_count {
            return Err(Error::TooFewShards)
        }

        Self::check_option_shards(shards)?;

        let shard_size = Self::option_shards_size(shards).unwrap();

	      // Quick check: are all of the shards present?  If so, there's
	      // nothing to do.
        let mut number_present = 0;
        let mut shard_present  = Vec::with_capacity(shards.len());
        for shard in shards.iter() {
            match *shard {
                None    => { shard_present.push(false); },
                Some(_) => { number_present += 1;
                             shard_present.push(true); }
            }
        }
        if number_present == self.data_shard_count {
            // Cool.  All of the shards data data.  We don't
            // need to do anything.
            return Ok(())
        }

	      // More complete sanity check
	      if number_present < self.data_shard_count {
		        return Err(Error::TooFewShards)
	      }

        // Fill in new shards
        for i in 0..shards.len() {
            if !shard_present[i] {
                shards[i] = Some(make_blank_shard(shard_size));
            }
        }

        let mut slices =
            Self::mut_option_shards_to_mut_slices(shards);

        self.reconstruct_internal(&mut slices,
                                  &shard_present,
                                  data_only)
    }

    fn reconstruct_internal(&self,
                            slices        : &mut [&mut [u8]],
                            slice_present : &[bool],
                            data_only     : bool) -> Result<(), Error> {
        if slices.len() < self.total_shard_count {
            return Err(Error::TooFewShards);
        }

        Self::check_mut_slices(slices)?;

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
        if number_present == self.data_shard_count {
            // Cool.  All of the shards data data.  We don't
            // need to do anything.
            return Ok(())
        }

	      // More complete sanity check
	      if number_present < self.data_shard_count {
		        return Err(Error::TooFewShards)
	      }

	      // Pull out an array holding just the shards that
	      // correspond to the rows of the submatrix.  These shards
	      // will be the input to the decoding process that re-creates
	      // the missing data shards.
	      //
	      // Also, create an array of indices of the valid rows we do have
	      // and the invalid rows we don't have up until we have enough valid rows.
        let mut sub_shards             : Vec<&[u8]> =
            Vec::with_capacity(self.data_shard_count);
        let mut leftover_parity_shards : Vec<&[u8]> =
            Vec::with_capacity(self.parity_shard_count);
        let mut missing_data_slices    : Vec<&mut [u8]> =
            Vec::with_capacity(self.parity_shard_count);
        let mut missing_parity_slices  : Vec<&mut [u8]> =
            Vec::with_capacity(self.parity_shard_count);
        let mut valid_indices          : Vec<usize> =
            Vec::with_capacity(self.data_shard_count);
        let mut invalid_indices        : Vec<usize> =
            Vec::with_capacity(self.data_shard_count);
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
                if sub_shards.len() < self.data_shard_count {
                    missing_data_slices.push(slice);
                } else {
                    missing_parity_slices.push(slice);
                }
                invalid_indices.push(matrix_row);
            }

            i          += 1;
            matrix_row += 1;
        }

        // Attempt to get the cached inverted matrix out of the tree
        // based on the indices of the invalid rows.
        let data_decode_matrix =
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
                    data_decode_matrix
                },
                Some(m) => {
                    m
                }
            };

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
	          // data shards were missing.
            let mut matrix_rows =
                Vec::with_capacity(self.parity_shard_count);
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
                let mut all_data_slices : Vec<&[u8]> =
                    Vec::with_capacity(self.data_shard_count);
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

    /*
#[cfg(test)]
mod tests {
    extern crate rand;

    use super::*;
    use self::rand::{thread_rng, Rng};
    //use std::rc::Rc;

    macro_rules! make_random_shards {
        ($per_shard:expr, $size:expr) => {{
            let mut shards = Vec::with_capacity(13);
            for _ in 0..$size {
                shards.push(make_blank_shard($per_shard));
            }

            for s in shards.iter_mut() {
                fill_random(s);
            }

            shards
        }}
    }

    fn assert_eq_shards(s1 : &Vec<Shard>, s2 : &Vec<Shard>) {
        assert_eq!(s1.len(), s2.len());
        for i in 0..s1.len() {
            assert_eq!(*s1[i].read().unwrap(),
                       *s2[i].read().unwrap());
        }
    }

    /*fn is_increasing_and_contains_data_row(indices : &Vec<usize>) -> bool {
        let cols = indices.len();
        for i in 0..cols-1 {
            if indices[i] >= indices[i+1] {
                return false
            }
        }
        return indices[0] < cols
    }*/

    /*fn increment_indices(indices : &mut Vec<usize>,
                         index_bound : usize) -> bool {
        for i in (0..indices.len()).rev() {
            indices[i] += 1;
            if indices[i] < index_bound {
                break;
            }

            if i == 0 {
                return false
            }

            indices[i] = 0
        }

        return true
    }*/

    /*fn increment_indices_until_increasing_and_contains_data_row(indices : &mut Vec<usize>, max_index : usize) -> bool {
        loop {
            let valid = increment_indices(indices, max_index);
            if !valid {
                return false
            }

            if is_increasing_and_contains_data_row(indices) {
                return true
            }
        }
    }*/

    /*fn find_singular_sub_matrix(m : Matrix) -> Option<Matrix> {
        let rows = m.row_count();
        let cols = m.column_count();
        let mut row_indices = Vec::with_capacity(cols);
        while increment_indices_until_increasing_and_contains_data_row(&mut row_indices, rows) {
            let mut sub_matrix = Matrix::new(cols, cols);
            for i in 0..row_indices.len() {
                let r = row_indices[i];
                for c in 0..cols {
                    sub_matrix.set(i, c, m.get(r, c));
                }
            }

            match sub_matrix.invert() {
                Err(matrix::Error::SingularMatrix) => return Some(sub_matrix),
                whatever => whatever.unwrap()
            };
        }
        None
    }*/

    fn fill_random(arr : &mut Shard) {
        for a in arr.write().unwrap().iter_mut() {
            *a = rand::random::<u8>();
        }
    }

    fn assert_eq_shards_with_range(shards1    : &Vec<Shard>,
                                   shards2    : &Vec<Shard>,
                                   offset     : usize,
                                   byte_count : usize) {
        for s in 0..shards1.len() {
            let slice1 = &shards1[s].read().unwrap()[offset..offset + byte_count];
            let slice2 = &shards2[s].read().unwrap()[offset..offset + byte_count];
            assert_eq!(slice1, slice2);
        }
    }

    #[test]
    #[should_panic]
    fn test_no_data_shards() {
        ReedSolomon::new(0, 1); }

    #[test]
    #[should_panic]
    fn test_no_parity_shards() {
        ReedSolomon::new(1, 0); }

    #[test]
    fn test_shard_count() {
        let mut rng = thread_rng();
        for _ in 0..10 {
            let data_shard_count   = rng.gen_range(1, 128);
            let parity_shard_count = rng.gen_range(1, 128);

            let total_shard_count = data_shard_count + parity_shard_count;

            let r = ReedSolomon::new(data_shard_count, parity_shard_count);

            assert_eq!(data_shard_count,   r.data_shard_count());
            assert_eq!(parity_shard_count, r.parity_shard_count());
            assert_eq!(total_shard_count,  r.total_shard_count());
        }
    }

    #[test]
    #[should_panic]
    fn test_calc_byte_count_byte_count_is_zero_case1() {
        let shards = make_random_shards!(1_000, 1);

        helper::calc_byte_count(&shards,
                                Some(0)); }

    #[test]
    #[should_panic]
    fn test_calc_byte_count_byte_count_is_zero_case2() {
        let shards = make_random_shards!(1_000, 0);

        helper::calc_byte_count(&shards,
                                None); }

    #[test]
    #[should_panic]
    fn test_calc_byte_count_option_shards_byte_count_is_zero_case1() {
        let shards = make_random_shards!(1_000, 1);
        let option_shards = shards_into_option_shards(shards);

        helper::calc_byte_count_option_shards(&option_shards,
                                              Some(0)); }

    #[test]
    #[should_panic]
    fn test_calc_byte_count_option_shards_byte_count_is_zero_case2() {
        let shards = make_random_shards!(1_000, 0);
        let option_shards = shards_into_option_shards(shards);

        helper::calc_byte_count_option_shards(&option_shards,
                                              None); }

    #[test]
    #[should_panic]
    fn test_calc_byte_count_option_shards_no_shards_present() {
        let shards = make_random_shards!(1_000, 2);

        let mut option_shards = shards_into_option_shards(shards);

        option_shards[0] = None;
        option_shards[1] = None;

        helper::calc_byte_count_option_shards(&option_shards,
                                              None); }

    #[test]
    fn test_shards_into_option_shards_into_shards() {
        for _ in 0..100 {
            let shards = make_random_shards!(1_000, 10);
            let expect = shards.clone();
            let inter  = shards_into_option_shards(shards);
            let result = option_shards_into_shards(inter);

            assert_eq_shards(&expect, &result);
        }
    }

    #[test]
    fn test_shards_to_option_shards_to_shards() {
        for _ in 0..100 {
            let shards = make_random_shards!(1_000, 10);
            let expect = shards.clone();
            let option_shards =
                shards_to_option_shards(&shards);
            let result        =
                option_shards_to_shards(&option_shards, None, None);

            assert_eq_shards(&expect, &result);
        }
    }

    #[test]
    #[should_panic]
    fn test_option_shards_to_shards_missing_shards_case1() {
        let shards = make_random_shards!(1_000, 10);
        let mut option_shards = shards_into_option_shards(shards);

        option_shards[0] = None;

        option_shards_to_shards(&option_shards, None, None);
    }

    #[test]
    fn test_option_shards_to_shards_missing_shards_case2() {
        let shards = make_random_shards!(1_000, 10);
        let mut option_shards = shards_into_option_shards(shards);

        option_shards[0] = None;
        option_shards[9] = None;

        option_shards_to_shards(&option_shards, Some(1), Some(8));
    }

    #[test]
    #[should_panic]
    fn test_option_shards_into_missing_shards() {
        let shards = make_random_shards!(1_000, 10);
        let mut option_shards = shards_into_option_shards(shards);

        option_shards[2] = None;

        option_shards_into_shards(option_shards);
    }

    #[test]
    #[should_panic]
    fn test_option_shards_to_shards_too_few_shards() {
        let shards = make_random_shards!(1_000, 10);
        let option_shards = shards_into_option_shards(shards);

        option_shards_to_shards(&option_shards,
                                None,
                                Some(11));
    }

    #[test]
    fn test_reedsolomon_clone() {
        let r1 = ReedSolomon::new(10, 3);
        let r2 = r1.clone();

        assert_eq!(r1, r2);
    }

    #[test]
    #[should_panic]
    fn test_reedsolomon_too_many_shards() {
        ReedSolomon::new(256, 1); }

    #[test]
    #[should_panic]
    fn test_check_buffer_and_sizes_total_shard_count() {
        let r = ReedSolomon::new(10, 3);
        let shards = make_random_shards!(1_000, 12);

        r.check_buffer_and_sizes(&shards, 0, 12);
    }

    #[test]
    #[should_panic]
    fn test_check_buffer_and_sizes_shards_same_size() {
        let r = ReedSolomon::new(3, 2);
        let shards = shards!([0, 1, 2],
                             [0, 1, 2, 4],
                             [0, 1, 2],
                             [0, 1, 2],
                             [0, 1, 2]);

        r.check_buffer_and_sizes(&shards, 0, 3);
    }

    #[test]
    #[should_panic]
    fn test_check_buffer_and_sizes_shards_too_small() {
        let r = ReedSolomon::new(3, 2);
        let shards = shards!([0, 1, 2],
                             [0, 1, 2],
                             [0, 1, 2],
                             [0, 1, 2],
                             [0, 1, 2]);

        r.check_buffer_and_sizes(&shards, 0, 4);
    }

    #[test]
    #[should_panic]
    fn test_check_buffer_and_sizes_option_shards_total_shard_count() {
        let r = ReedSolomon::new(10, 3);
        let shards =
            shards_into_option_shards(
                make_random_shards!(1_000, 12));

        r.check_buffer_and_sizes_option_shards(&shards, 0, 12);
    }

    #[test]
    #[should_panic]
    fn test_check_buffer_and_sizes_option_shards_shards_same_size() {
        let r = ReedSolomon::new(3, 2);
        let shards =
            shards_into_option_shards(
                shards!([0, 1, 2],
                        [0, 1, 2, 4],
                        [0, 1, 2],
                        [0, 1, 2],
                        [0, 1, 2]));

        r.check_buffer_and_sizes_option_shards(&shards, 0, 3);
    }

    #[test]
    #[should_panic]
    fn test_check_buffer_and_sizes_option_shards_shards_too_small() {
        let r = ReedSolomon::new(3, 2);
        let shards =
            shards_into_option_shards(
                shards!([0, 1, 2],
                        [0, 1, 2],
                        [0, 1, 2],
                        [0, 1, 2],
                        [0, 1, 2]));

        r.check_buffer_and_sizes_option_shards(&shards, 0, 4);
    }

    #[test]
    fn test_shallow_clone_shards() {
        let shards1 = make_random_shards!(1_000, 10);

        for v in shards1.iter() {
            assert_eq!(1, Arc::strong_count(v));
        }

        let shards2 = shards1.clone();

        for v in shards1.iter() {
            assert_eq!(2, Arc::strong_count(v));
        }
        for v in shards2.iter() {
            assert_eq!(2, Arc::strong_count(v));
        }
    }

    #[test]
    fn test_deep_clone_shards() {
        let shards1 = make_random_shards!(1_000, 10);

        for v in shards1.iter() {
            assert_eq!(1, Arc::strong_count(v));
        }

        let shards2 = deep_clone_shards(&shards1);

        for v in shards1.iter() {
            assert_eq!(1, Arc::strong_count(v));
        }
        for v in shards2.iter() {
            assert_eq!(1, Arc::strong_count(v));
        }
    }

    #[test]
    fn test_shallow_clone_option_shards() {
        let shards1 =
            shards_into_option_shards(
                make_random_shards!(1_000, 10));

        for v in shards1.iter() {
            if let Some(ref x) = *v {
                assert_eq!(1, Arc::strong_count(x));
            }
        }

        let shards2 = shards1.clone();

        for v in shards1.iter() {
            if let Some(ref x) = *v {
                assert_eq!(2, Arc::strong_count(x));
            }
        }
        for v in shards2.iter() {
            if let Some(ref x) = *v {
                assert_eq!(2, Arc::strong_count(x));
            }
        }
    }

    #[test]
    fn test_deep_clone_option_shards() {
        let mut shards1 =
            shards_into_option_shards(
                make_random_shards!(1_000, 10));

        for v in shards1.iter() {
            if let Some(ref x) = *v {
                assert_eq!(1, Arc::strong_count(x));
            }
        }

        let shards2 = deep_clone_option_shards(&shards1);

        for v in shards1.iter() {
            if let Some(ref x) = *v {
                assert_eq!(1, Arc::strong_count(x));
            }
        }
        for v in shards2.iter() {
            if let Some(ref x) = *v {
                assert_eq!(1, Arc::strong_count(x));
            }
        }

        shards1[0] = None;
        shards1[4] = None;
        shards1[7] = None;

        let shards3 = deep_clone_option_shards(&shards1);

        if let Some(_) = shards3[0] { panic!() }
        if let Some(_) = shards3[4] { panic!() }
        if let Some(_) = shards3[7] { panic!() }
    }

    #[test]
    fn test_rc_counts_carries_over_decode_missing() {
        let r = ReedSolomon::new(3, 2);

        let mut master_copy = shards!([0, 1,  2,  3],
                                      [4, 5,  6,  7],
                                      [8, 9, 10, 11],
                                      [0, 0,  0,  0],
                                      [0, 0,  0,  0]);

        r.encode_parity(&mut master_copy, None, None);

        // the cloning below increases rc counts from 1 to 2
        let mut shards = shards_into_option_shards(master_copy.clone());

        shards[0] = None;
        shards[4] = None;

        // the new shards constructed by decode_missing
        // will have rc count of just 1
        r.decode_missing(&mut shards, None, None).unwrap();
        
        let result = option_shards_into_shards(shards);
        
        assert!(r.is_parity_correct(&result, None, None));
        assert_eq!(1, Arc::strong_count(&result[0]));
        assert_eq!(2, Arc::strong_count(&result[1]));
        assert_eq!(2, Arc::strong_count(&result[2]));
        assert_eq!(2, Arc::strong_count(&result[3]));
        assert_eq!(1, Arc::strong_count(&result[4]));
    }

    #[test]
    fn test_shards_to_option_shards_does_not_change_rc_counts() {
        let shards = make_random_shards!(1_000, 10);

        let option_shards =
            shards_to_option_shards(&shards);

        for v in shards.iter() {
            assert_eq!(1, Arc::strong_count(v));
        }
        for v in option_shards.iter() {
            if let Some(ref x) = *v {
                assert_eq!(1, Arc::strong_count(x));
            }
        }
    }

    #[test]
    fn test_shards_into_option_shards_does_not_change_rc_counts() {
        let shards = make_random_shards!(1_000, 10);

        let option_shards =
            shards_into_option_shards(shards);

        for v in option_shards.iter() {
            if let Some(ref x) = *v {
                assert_eq!(1, Arc::strong_count(x));
            }
        }
    }

    #[test]
    fn test_option_shards_to_shards_does_not_change_rc_counts() {
        let option_shards =
            shards_to_option_shards(
                &make_random_shards!(1_000, 10));

        let shards =
            option_shards_to_shards(&option_shards, None, None);

        for v in option_shards.iter() {
            if let Some(ref x) = *v {
                assert_eq!(1, Arc::strong_count(x));
            }
        }
        for v in shards.iter() {
            assert_eq!(1, Arc::strong_count(v));
        }
    }

    #[test]
    fn test_option_shards_into_shards_does_not_change_rc_counts() {
        let option_shards =
            shards_to_option_shards(
                &make_random_shards!(1_000, 10));

        let shards =
            option_shards_into_shards(option_shards);

        for v in shards.iter() {
            assert_eq!(1, Arc::strong_count(v));
        }
    }

    #[test]
    fn test_encoding() {
        let per_shard = 50_000;

        let r = ReedSolomon::new(10, 3);

        let mut shards = make_random_shards!(per_shard, 13);

        r.encode_parity(&mut shards, None, None);
        assert!(r.is_parity_correct(&shards, None, None));
    }

    #[test]
    fn test_encoding_with_range() {
        let per_shard = 50_000;

        let r = ReedSolomon::new(10, 3);

        let mut shards = make_random_shards!(per_shard, 13);

        r.encode_parity(&mut shards, Some(7), Some(100));
        assert!(r.is_parity_correct(&shards, Some(7), Some(100)));
    }

    #[test]
    fn test_decode_missing() {
        let per_shard = 100_000;

        let r = ReedSolomon::new(8, 5);

        let mut shards = make_random_shards!(per_shard, 13);

        r.encode_parity(&mut shards, None, None);

        let master_copy = shards.clone();

        let mut shards = shards_to_option_shards(&shards);

        // Try to decode with all shards present
        r.decode_missing(&mut shards,
                         None, None).unwrap();
        {
            let shards = option_shards_to_shards(&shards, None, None);
            assert!(r.is_parity_correct(&shards, None, None));
            assert_eq_shards(&shards, &master_copy);
        }

        // Try to decode with 10 shards
        shards[0] = None;
        shards[2] = None;
        //shards[4] = None;
        r.decode_missing(&mut shards,
                         None, None).unwrap();
        {
            let shards = option_shards_to_shards(&shards, None, None);
            assert!(r.is_parity_correct(&shards, None, None));
            assert_eq_shards(&shards, &master_copy);
        }

        // Try to deocde with 6 data and 4 parity shards
        shards[0] = None;
        shards[2] = None;
        shards[12] = None;
        r.decode_missing(&mut shards,
                         None, None).unwrap();
        {
            let shards = option_shards_to_shards(&shards, None, None);
            assert!(r.is_parity_correct(&shards, None, None));
        }

        // Try to decode with 7 data and 1 parity shards
        shards[0] = None;
        shards[1] = None;
        shards[9] = None;
        shards[10] = None;
        shards[11] = None;
        shards[12] = None;
        assert_eq!(r.decode_missing(&mut shards,
                                    None, None).unwrap_err(),
                   Error::NotEnoughShards);
    }

    #[test]
    fn test_decode_missing_with_range() {
        let per_shard = 100_000;

        let offset = 7;
        let byte_count = 100;
        let op_offset = Some(offset);
        let op_byte_count = Some(byte_count);

        let r = ReedSolomon::new(8, 5);

        let mut shards = make_random_shards!(per_shard, 13);

        r.encode_parity(&mut shards, Some(7), Some(100));

        let master_copy = shards.clone();

        let mut shards = shards_to_option_shards(&shards);

        // Try to decode with all shards present
        r.decode_missing(&mut shards,
                         Some(7), Some(100)).unwrap();
        {
            let shards = option_shards_to_shards(&shards, None, None);
            assert!(r.is_parity_correct(&shards, op_offset, op_byte_count));
            assert_eq_shards_with_range(&shards, &master_copy, offset, byte_count);
        }

        // Try to decode with 10 shards
        shards[0] = None;
        shards[2] = None;
        //shards[4] = None;
        r.decode_missing(&mut shards,
                         op_offset, op_byte_count).unwrap();
        {
            let shards = option_shards_to_shards(&shards, None, None);
            assert!(r.is_parity_correct(&shards, op_offset, op_byte_count));
            assert_eq_shards_with_range(&shards, &master_copy, offset, byte_count);
        }

        // Try to deocde with 6 data and 4 parity shards
        shards[0] = None;
        shards[2] = None;
        shards[12] = None;
        r.decode_missing(&mut shards,
                         None, None).unwrap();
        {
            let shards = option_shards_to_shards(&shards, None, None);
            assert!(r.is_parity_correct(&shards, op_offset, op_byte_count));
            assert_eq_shards_with_range(&shards, &master_copy, offset, byte_count);
        }

        // Try to decode with 7 data and 1 parity shards
        shards[0] = None;
        shards[1] = None;
        shards[9] = None;
        shards[10] = None;
        shards[11] = None;
        shards[12] = None;
        assert_eq!(r.decode_missing(&mut shards,
                                    op_offset, op_byte_count).unwrap_err(),
                   Error::NotEnoughShards);
    }

    #[test]
    fn test_is_parity_correct() {
        let per_shard = 33_333;

        let r = ReedSolomon::new(10, 4);

        let mut shards = make_random_shards!(per_shard, 14);

        r.encode_parity(&mut shards, None, None);
        assert!(r.is_parity_correct(&shards, None, None));

        // corrupt shards
        fill_random(&mut shards[5]);
        assert!(!r.is_parity_correct(&shards, None, None));

        // Re-encode
        r.encode_parity(&mut shards, None, None);
        fill_random(&mut shards[1]);
        assert!(!r.is_parity_correct(&shards, None, None));
    }

    #[test]
    fn test_is_parity_correct_with_range() {
        let per_shard = 33_333;

        let offset = 7;
        let byte_count = 100;
        let op_offset = Some(offset);
        let op_byte_count = Some(byte_count);

        let r = ReedSolomon::new(10, 4);

        let mut shards = make_random_shards!(per_shard, 14);

        r.encode_parity(&mut shards, op_offset, op_byte_count);
        assert!(r.is_parity_correct(&shards, op_offset, op_byte_count));

        // corrupt shards
        fill_random(&mut shards[5]);
        assert!(!r.is_parity_correct(&shards, op_offset, op_byte_count));

        // Re-encode
        r.encode_parity(&mut shards, op_offset, op_byte_count);
        fill_random(&mut shards[1]);
        assert!(!r.is_parity_correct(&shards, op_offset, op_byte_count));
    }

    #[test]
    fn test_one_encode() {
        let r = ReedSolomon::new(5, 5);

        let mut shards = shards!([0, 1],
                                 [4, 5],
                                 [2, 3],
                                 [6, 7],
                                 [8, 9],
                                 [0, 0],
                                 [0, 0],
                                 [0, 0],
                                 [0, 0],
                                 [0, 0]);

        r.encode_parity(&mut shards, None, None);
        { assert_eq!(shards[5].read().unwrap()[0], 12);
          assert_eq!(shards[5].read().unwrap()[1], 13); }
        { assert_eq!(shards[6].read().unwrap()[0], 10);
          assert_eq!(shards[6].read().unwrap()[1], 11); }
        { assert_eq!(shards[7].read().unwrap()[0], 14);
          assert_eq!(shards[7].read().unwrap()[1], 15); }
        { assert_eq!(shards[8].read().unwrap()[0], 90);
          assert_eq!(shards[8].read().unwrap()[1], 91); }
        { assert_eq!(shards[9].read().unwrap()[0], 94);
          assert_eq!(shards[9].read().unwrap()[1], 95); }

        assert!(r.is_parity_correct(&shards, None, None));

        shards[8].write().unwrap()[0] += 1;
        assert!(!r.is_parity_correct(&shards, None, None));
    }
}
*/
