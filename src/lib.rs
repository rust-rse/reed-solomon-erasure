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
//use std::ops::{Deref, DerefMut};
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

pub fn mut_refs_to_refs<'a>(slices : &'a [&mut [u8]]) -> Vec<&'a [u8]> {
    let mut result : Vec<&[u8]> =
        Vec::with_capacity(slices.len());
    for slice in slices.into_iter() {
        result.push(slice);
    }
    result
}

pub fn shards_to_slices<'a>(shards : &'a Vec<Shard>) -> Vec<&'a [u8]> {
    let mut result : Vec<&[u8]> =
        Vec::with_capacity(shards.len());
    for shard in shards.into_iter() {
        result.push(shard);
    }
    result
}

pub fn mut_shards_to_mut_slices(shards : &mut [Shard])
                            -> Vec<&mut [u8]> {
    let mut result : Vec<&mut [u8]> =
        Vec::with_capacity(shards.len());
    for shard in shards.into_iter() {
        result.push(shard);
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

    pub fn encode_shards(&self,
                         shards : &mut [Shard]) -> Result<(), Error> {
        if shards.len() < self.total_shard_count {
            return Err(Error::TooFewShards);
        }

        let mut slices = mut_shards_to_mut_slices(shards);

        self.encode(&mut slices)
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
