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
mod galois;
mod matrix;

extern crate rayon;
use rayon::prelude::*;
//extern crate threadpool;
//use threadpool::ThreadPool;

extern crate num_cpus;

use matrix::Matrix;

#[derive(PartialEq, Debug)]
pub enum Error {
    NotEnoughShards
}

/// Main data type used by this library
//pub type Shard = Rc<RefCell<Box<[u8]>>>;

pub type Shard = Box<[u8]>;

/*fn shard_into_tsshard(shard : Shard) -> TsShard {
    let inner : Box<[u8]> = shard.into_inner();
    Arc::new(RwLock::new(inner))
}

fn tsshard_into_shard(shard : TsShard) -> Shard {
    let inner : Box<[u8]> = shard.into_inner().unwrap();
    Rc::new(RefCell::new(inner))
}

fn shards_into_tsshards(shards : Vec<Shard>) -> Vec<TsShard> {
    let mut result = Vec::with_capacity(shards.len());
    for shard in shards.into_iter() {
        let inner : Box<[u8]> =
            shard.into_inner();
        result.push(Arc::new(RwLock::new(inner)));
    }
    result
}

fn tsshards_into_shards(shards : Vec<TsShard>) -> Vec<Shard> {
    let mut result = Vec::with_capacity(shards.len());
    for shard in shards.into_iter() {
        let inner : Box<[u8]> =
            shard.into_inner().unwrap();
        result.push(Rc::new(RefCell::new(inner)));
    }
    result
}*/
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
        boxed_u8_into_shard(Box::new([ $( $x ),* ]))
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
        vec![ $( boxed_u8_into_shard(Box::new([ $( $x ),* ])) ),* ]
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

    pub fn split_slice_mut_with_index<'a, T> (slice      : &'a mut [T],
                                              chunk_size : usize)
                                              -> Vec<(usize, &'a mut [T])> {
        fn helper<'a, T>(slice      : &'a mut [T],
                         chunk_size : usize,
                         cur_index  : usize,
                         mut result : Vec<(usize, &'a mut [T])>)
                         -> Vec<(usize, &'a mut [T])> {
            if chunk_size < slice.len() {
                let (l, r) = slice.split_at_mut(chunk_size);
                result.push((cur_index, l));
                helper(r, chunk_size, cur_index + 1, result)
            }
            else {
                result.push((cur_index, slice));
                result
            }
        }

        let result = Vec::with_capacity(slice.len() / chunk_size + 1);
        helper(slice,
               chunk_size,
               0,
               result)
    }

    pub fn split_slice_with_index<'a, T> (slice      : &'a [T],
                                          chunk_size : usize)
                                          -> Vec<(usize, &'a [T])> {
        fn helper<'a, T>(slice      : &'a [T],
                         chunk_size : usize,
                         cur_index  : usize,
                         mut result : Vec<(usize, &'a [T])>)
                         -> Vec<(usize, &'a [T])> {
            if chunk_size < slice.len() {
                let (l, r) = slice.split_at(chunk_size);
                result.push((cur_index, l));
                helper(r, chunk_size, cur_index + 1, result)
            }
            else {
                result.push((cur_index, slice));
                result
            }
        }

        let result = Vec::with_capacity(slice.len() / chunk_size + 1);
        helper(slice,
               chunk_size,
               0,
               result)
    }

    pub fn break_down_slice_mut<'a, T>(slice : &'a mut [T]) -> Vec<&'a mut T> {
        let mut result = Vec::with_capacity(slice.len());
        for (_, v) in split_slice_mut_with_index(slice, 1).into_iter() {
            result.push(&mut v[0]);
        }
        result
    }

    pub fn break_down_slice<'a, T>(slice : &'a [T]) -> Vec<&'a T> {
        let mut result = Vec::with_capacity(slice.len());
        for (_, v) in split_slice_with_index(slice, 1).into_iter() {
            result.push(& v[0]);
        }
        result
    }

    pub fn slice_to_vec_of_refs<'a, T>(slice : &'a [T]) -> Vec<&'a T> {
        let mut result = Vec::with_capacity(slice.len());
        for v in slice.iter() {
            result.push(v);
        }
        result
    }

    pub fn mut_slice_to_vec_of_mut_refs<'a, T>(slice : &'a mut [T])
                                               -> Vec<&'a mut T> {
        let mut result = Vec::with_capacity(slice.len());
        for v in slice.iter_mut() {
            result.push(v);
        }
        result
    }
}


pub fn boxed_u8_into_shard(b : Box<[u8]>) -> Shard {
    b
}

/// Makes shard with byte array of zero length
pub fn make_zero_len_shard() -> Shard {
    boxed_u8_into_shard(Box::new([]))
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
        result.push(Some(boxed_u8_into_shard(inner)));
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
        result.push(boxed_u8_into_shard(inner));
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
        result.push(boxed_u8_into_shard(inner));
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
                             Some(boxed_u8_into_shard(inner)) },
            None        => None
        };
        result.push(inner);
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
#[derive(PartialEq, Debug)]
pub struct ReedSolomon {
    data_shard_count   : usize,
    parity_shard_count : usize,
    total_shard_count  : usize,
    matrix             : Matrix,
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

    fn check_buffer_and_sizes(&self,
                              shards : &Vec<Shard>,
                              offset : usize, byte_count : usize) {
        if shards.len() != self.total_shard_count {
            panic!("Incorrect number of shards : {}", shards.len())
        }

        let shard_length = shards[0].len();
        for shard in shards.iter() {
            if shard.len() != shard_length {
                panic!("Shards are of different sizes");
            }
        }

        if shard_length < offset + byte_count {
            panic!("Shards too small, shard length : Some({}), offset + byte_count : {}", shard_length, offset + byte_count);
        }
    }

    fn check_buffer_and_sizes_option_shards(&self,
                                            shards : &Vec<Option<Shard>>,
                                            offset : usize, byte_count : usize) {
        if shards.len() != self.total_shard_count {
            panic!("Incorrect number of shards : {}", shards.len())
        }

        let mut shard_length = None;
        for shard in shards.iter() {
            if let Some(ref s) = *shard {
                match shard_length {
                    None    => shard_length = Some(s.len()),
                    Some(x) => {
                        if s.len() != x {
                            panic!("Shards are of different sizes");
                        }
                    }
                }
            }
        }

        if let Some(x) = shard_length {
            if x < offset + byte_count {
                panic!("Shards too small, shard length : Some({}), offset + byte_count : {}", x, offset + byte_count);
            }
        }
    }

    #[inline(always)]
    fn code_first_input_shard(pparam       : &ParallelParam,
                              matrix_rows  : &[&[u8]],
                              outputs      : &mut [&mut Shard],
                              output_count : usize,
                              offset       : usize,
                              byte_count   : usize,
                              i_input      : usize,
                              input_shard  : &Shard) {
        let table = &galois::MULT_TABLE;

        for i_output in 0..output_count {
            let output_shard =
                &mut outputs[i_output];
            let matrix_row       = matrix_rows[i_output];
            let mult_table_row   = table[matrix_row[i_input] as usize];
            let output_shard_pieces =
                helper::split_slice_mut_with_index(
                    &mut output_shard[offset..offset + byte_count],
                    pparam.bytes_per_encode);
            output_shard_pieces.into_par_iter()
                .for_each(|(index, out)| {
                    let slice_offset =
                        offset + index * pparam.bytes_per_encode;
                    for i in 0..out.len() {
                        out[i] =
                            mult_table_row[
                                input_shard[slice_offset + i] as usize];
                    }
                })
        }
    }

    #[inline(always)]
    fn code_other_input_shard(pparam       : &ParallelParam,
                              matrix_rows  : &[&[u8]],
                              outputs      : &mut [&mut Shard],
                              output_count : usize,
                              offset       : usize,
                              byte_count   : usize,
                              i_input      : usize,
                              input_shard  : &Shard) {
        let table = &galois::MULT_TABLE;

        for i_output in 0..output_count {
            let output_shard =
                &mut outputs[i_output];
            let matrix_row       = matrix_rows[i_output];
            let mult_table_row   = &table[matrix_row[i_input] as usize];
            let output_shard_pieces =
                helper::split_slice_mut_with_index(
                    &mut output_shard[offset..offset + byte_count],
                    pparam.bytes_per_encode);
            output_shard_pieces.into_par_iter()
                .for_each(|(index, out)| {
                    let slice_offset =
                        offset + index * pparam.bytes_per_encode;
                    for i in 0..out.len() {
                        out[i] ^=
                            mult_table_row[
                                input_shard[slice_offset + i] as usize];
                    }
                })
        }
    }

    // Translated from InputOutputByteTableCodingLoop.java
    fn code_some_shards(pparam       : &ParallelParam,
                        matrix_rows  : &Vec<&[u8]>,
                        inputs       : &[&Shard],
                        input_count  : usize,
                        outputs      : &mut [&mut Shard],
                        output_count : usize,
                        offset       : usize,
                        byte_count   : usize) {
        {
            let i_input = 0;
            let input_shard = inputs[i_input];
            Self::code_first_input_shard(pparam,
                                         matrix_rows,
                                         outputs, output_count,
                                         offset,  byte_count,
                                         i_input, &input_shard);
        }

        for i_input in 1..input_count {
            let input_shard = &inputs[i_input];
            Self::code_other_input_shard(pparam,
                                         matrix_rows,
                                         outputs, output_count,
                                         offset, byte_count,
                                         i_input, &input_shard);
        }
    }

    fn code_some_option_shards(pparam       : &ParallelParam,
                               matrix_rows  : &[&[u8]],
                               inputs       : &[Option<&Shard>],
                               input_count  : usize,
                               outputs      : &mut [&mut Shard],
                               output_count : usize,
                               offset       : usize,
                               byte_count   : usize) {
        {
            let i_input = 0;
            let input_shard = match inputs[i_input] {
                Some(ref x) => x,
                None        => panic!()
            };
            Self::code_first_input_shard(pparam,
                                         matrix_rows,
                                         outputs, output_count,
                                         offset,  byte_count,
                                         i_input, &input_shard);
        }

        for i_input in 1..input_count {
            let input_shard = match inputs[i_input] {
                Some(ref x) => x,
                None        => panic!()
            };
            Self::code_other_input_shard(pparam,
                                         matrix_rows,
                                         outputs, output_count,
                                         offset, byte_count,
                                         i_input, &input_shard);
        }
    }

    /// Constructs parity shards
    ///
    /// # Remarks
    ///
    /// This overwrites data in the parity shard slots.
    ///
    /// Panics when the shards are of different sizes, number of shards does not match codec's configuration, or when the shards' length is shorter than required.
    pub fn encode_parity(&self,
                         shards     : &mut Vec<Shard>,
                         offset     : Option<usize>,
                         byte_count : Option<usize>) {
        let (offset, byte_count) =
            helper::calc_offset_and_byte_count(offset, shards, byte_count);

        self.check_buffer_and_sizes(shards, offset, byte_count);

        let (inputs, outputs) = shards.split_at_mut(self.data_shard_count);
        let inputs_ref  = helper::slice_to_vec_of_refs(inputs);
        let mut outputs_ref = helper::mut_slice_to_vec_of_mut_refs(outputs);

        let parity_rows = self.get_parity_rows();

        Self::code_some_shards(&self.pparam,
                               &parity_rows,
                               &inputs_ref,  self.data_shard_count,
                               &mut outputs_ref, self.parity_shard_count,
                               offset, byte_count);
    }

    // Translated from CodingLoopBase.java
    fn check_some_shards(matrix_rows : &Vec<&[u8]>,
                         inputs      : &[Shard],
                         input_count : usize,
                         to_check    : &[Shard],
                         check_count : usize,
                         offset      : usize,
                         byte_count  : usize)
                         -> bool {
        let table = &galois::MULT_TABLE;

        for i_byte in offset..offset + byte_count {
            for i_output in 0..check_count {
                let matrix_row = matrix_rows[i_output as usize].clone();
                let mut value = 0;
                for i_input in 0..input_count {
                    value ^=
                        table
                        [matrix_row[i_input]     as usize]
                        [inputs[i_input][i_byte] as usize];
                }
                if to_check[i_output][i_byte] != value {
                    return false
                }
            }
        }
        true
    }

    /// Verify correctness of parity shards
    pub fn is_parity_correct(&self,
                             shards     : &Vec<Shard>,
                             offset     : Option<usize>,
                             byte_count : Option<usize>) -> bool {
        let (offset, byte_count) =
            helper::calc_offset_and_byte_count(offset,
                                               shards,
                                               byte_count);

        self.check_buffer_and_sizes(shards, offset, byte_count);

        let (data_shards, to_check) = shards.split_at(self.data_shard_count);

        let parity_rows = self.get_parity_rows();

        Self::check_some_shards(&parity_rows,
                                data_shards, self.data_shard_count,
                                to_check,    self.parity_shard_count,
                                offset, byte_count)
    }

    /// Reconstruct missing shards
    ///
    /// # Remarks
    ///
    /// Panics when the shards are of different sizes, number of shards does not match codec's configuration, or when the shards' length is shorter than required.
    pub fn decode_missing(&self,
                          shards     : &mut Vec<Option<Shard>>,
                          offset     : Option<usize>,
                          byte_count : Option<usize>)
                          -> Result<(), Error> {
        let (offset, byte_count) =
            helper::calc_offset_and_byte_count_option_shards(offset,
                                                             shards,
                                                             byte_count);

        self.check_buffer_and_sizes_option_shards(shards, offset, byte_count);

        let shard_length = helper::calc_byte_count_option_shards(&shards,
                                                                 None);

        // Quick check: are all of the shards present?  If so, there's
        // nothing to do.
        // And also note down which shards are present
        let mut shard_present  = Vec::with_capacity(shards.len());
        for v in shards.iter() {
            match *v {
                Some(_) => { shard_present.push(true); },
                None    => { shard_present.push(false); }
            }
        }
        let mut data_shards_present   = 0;
        let mut parity_shards_present = 0;
        for i in 0..shard_present.len() {
            if shard_present[i] {
                if i < self.data_shard_count { data_shards_present   += 1; }
                else                         { parity_shards_present += 1; }
            }
        }
        let number_present = data_shards_present + parity_shards_present;
        if number_present == self.total_shard_count {
            // Cool.  All of the shards data data.  We don't
            // need to do anything.
            return Ok(())
        }

        // More complete sanity check
        if number_present < self.data_shard_count {
            return Err(Error::NotEnoughShards)
        }

        // Pull out the rows of the matrix that correspond to the
        // shards that we have and build a square matrix.  This
        // matrix could be used to generate the shards that we have
        // from the original data.
        //
        // Also, pull out an array holding just the shards that
        // correspond to the rows of the submatrix.  These shards
        // will be the input to the decoding process that re-creates
        // the missing data shards.
        let mut sub_matrix =
            Matrix::new(self.data_shard_count, self.data_shard_count);
        let mut sub_shards : Vec<&Shard> =
            Vec::with_capacity(self.data_shard_count);
        let mut matrix_row = 0;
        let data_shard_count = self.data_shard_count;
        let matrix           = &self.matrix;
        for shard in shards.iter() {
            let sub_matrix_row = sub_shards.len();

            if sub_matrix_row >= self.data_shard_count { break; }

            if let Some(ref shard) = *shard {
                for c in 0..self.data_shard_count {
                    sub_matrix.set(sub_matrix_row, c,
                                   matrix.get(matrix_row, c));
                }
                sub_shards.push(shard);
            }

            matrix_row += 1;
        }

        // Invert the matrix, so we can go from the encoded shards
        // back to the original data.  Then pull out the row that
        // generates the shard that we want to decode.  Note that
        // since this matrix maps back to the orginal data, it can
        // be used to create a data shard, but not a parity shard.
        let data_decode_matrix = sub_matrix.invert().unwrap();

        // Re-create any data shards that were missing.
        //
        // The input to the coding is all of the shards we actually
        // have, and the output is the missing data shards.  The computation
        // is done using the special decode matrix we just built.
        let mut matrix_rows : Vec<&[u8]> =
            Vec::with_capacity(self.parity_shard_count);
        {
            let mut new_data_shards : Vec<Shard> =
                Vec::with_capacity(self.data_shard_count - data_shards_present);
            {
                let mut new_data_shards_ref : Vec<&mut Shard> =
                    Vec::with_capacity(self.data_shard_count - data_shards_present);
                for i_shard in 0..self.data_shard_count {
                    if !shard_present[i_shard] {
                        new_data_shards.push(
                            make_blank_shard(shard_length));
                        matrix_rows.push(
                            data_decode_matrix.get_row(i_shard));
                    }
                }
                let new_data_shard_count = new_data_shards.len();
                for shard in new_data_shards.iter_mut() {
                    /*new_data_shards_ref.push(
                    &mut new_data_shards[new_data_shards.len() - 1]);*/
                    new_data_shards_ref.push(shard);
                }
                Self::code_some_shards(&self.pparam,
                                       &matrix_rows,
                                       &sub_shards,
                                       self.data_shard_count,
                                       &mut new_data_shards_ref,
                                       new_data_shard_count,
                                       offset, byte_count);
            }

            // Patch the total shard set using the new data shards
            // in reverse order(so moving actually works)
            let mut i_shard = 0;
            for new_shard in new_data_shards.into_iter() {
                if !shard_present[i_shard] {
                    shards[i_shard] = new_shard;
                }
                i_shard += 1;
            }
        }

        // Now that we have all of the data shards intact, we can
        // compute any of the parity that is missing.
        //
        // The input to the coding is ALL of the data shards, including
        // any that we just calculated.  The output is whichever of the
        // data shards were missing.
        {
            let parity_rows = self.get_parity_rows();
        }

        Ok(())
    }

    /*
    pub fn decode_missing(&self,
                          shards     : &mut Vec<Option<Shard>>,
                          offset     : Option<usize>,
                          byte_count : Option<usize>)
                          -> Result<(), Error> {
        let (offset, byte_count) =
            helper::calc_offset_and_byte_count_option_shards(offset,
                                                             shards,
                                                             byte_count);

        self.check_buffer_and_sizes_option_shards(shards, offset, byte_count);

        let shard_length = helper::calc_byte_count_option_shards(&shards,
                                                                 None);

        // Quick check: are all of the shards present?  If so, there's
        // nothing to do.
        let mut number_present = 0;
        let mut shard_present  = Vec::with_capacity(shards.len());
        for v in shards.iter() {
            match *v {
                Some(_) => { number_present += 1;
                             shard_present.push(true); },
                None    => { shard_present.push(false); }
            }
        }
        if number_present == self.total_shard_count {
            // Cool.  All of the shards data data.  We don't
            // need to do anything.
            return Ok(())
        }

        // More complete sanity check
        if number_present < self.data_shard_count {
            return Err(Error::NotEnoughShards)
        }

        let mut shards_ref = Vec::with_capacity(shards.len());
        for v in shards.iter_mut() {
            match *v {
                Some(ref x) => shards_ref.push(Some(x)),
                None        => shards_ref.push(None)
            }
        }

        // Pull out the rows of the matrix that correspond to the
        // shards that we have and build a square matrix.  This
        // matrix could be used to generate the shards that we have
        // from the original data.
        //
        // Also, pull out an array holding just the shards that
        // correspond to the rows of the submatrix.  These shards
        // will be the input to the decoding process that re-creates
        // the missing data shards.
        let mut sub_matrix =
            Matrix::new(self.data_shard_count, self.data_shard_count);
        let mut sub_shards : Vec<&Shard> =
            Vec::with_capacity(self.data_shard_count);
        {
            for matrix_row in 0..self.total_shard_count {
                let sub_matrix_row = sub_shards.len();

                if sub_matrix_row >= self.data_shard_count { break; }

                if let Some(shard) = shards_ref[matrix_row] {
                    for c in 0..self.data_shard_count {
                        sub_matrix.set(sub_matrix_row, c,
                                       self.matrix.get(matrix_row, c));
                    }
                    sub_shards.push(shard);
                }
            }
        }

        // Invert the matrix, so we can go from the encoded shards
        // back to the original data.  Then pull out the row that
        // generates the shard that we want to decode.  Note that
        // since this matrix maps back to the orginal data, it can
        // be used to create a data shard, but not a parity shard.
        let data_decode_matrix = sub_matrix.invert().unwrap();

        // Re-create any data shards that were missing.
        //
        // The input to the coding is all of the shards we actually
        // have, and the output is the missing data shards.  The computation
        // is done using the special decode matrix we just built.
        let mut matrix_rows : Vec<&[u8]> =
            vec![&[]; self.parity_shard_count];
        {
            let mut outputs : Vec<&mut Shard> =
                Vec::with_capacity(self.parity_shard_count);
            let mut output_count = 0;
            for i_shard in 0..self.data_shard_count {
                if !shard_present[i_shard] {
                    shards[i_shard] =
                        Some(make_blank_shard(shard_length));
                    match shards[i_shard] {
                        Some(ref mut x) => outputs.push(x),
                        None => panic!(),
                    }
                    matrix_rows[output_count] =
                        data_decode_matrix.get_row(i_shard);
                    output_count += 1;
                }
            }
            Self::code_some_shards(&self.pparam,
                                   &matrix_rows,
                                   sub_shards.as_slice(),  self.data_shard_count,
                                   &mut outputs, output_count,
                                   offset, byte_count);
        }

        /*
        // Now that we have all of the data shards intact, we can
        // compute any of the parity that is missing.
        //
        // The input to the coding is ALL of the data shards, including
        // any that we just calculated.  The output is whichever of the
        // parity shards were missing.
        let parity_rows = self.get_parity_rows();
        {
            let mut outputs : Vec<Shard> =
                make_blank_shards(shard_length,
                                  self.parity_shard_count);
            let mut output_count = 0;
            for i_shard in self.data_shard_count..self.total_shard_count {
                if let None = shards[i_shard] {
                    // link slot in outputs to the missing slot in shards
                    shards[i_shard] =
                        Some(&outputs[output_count]);
                    matrix_rows[output_count] =
                        parity_rows[i_shard
                                    - self.data_shard_count];
                    output_count += 1;
                }
            }

            Self::code_some_option_shards(&self.pparam,
                                          &matrix_rows,
                                          &shards, self.data_shard_count,
                                          &mut outputs, output_count,
                                          offset, byte_count);
        }*/

        Ok (())
    }
    */ 
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
