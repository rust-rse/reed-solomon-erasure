extern crate alloc;

use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;

use smallvec::SmallVec;

use crate::errors::Error;
use crate::errors::SBSError;

use crate::matrix::Matrix;

use lru::LruCache;

#[cfg(feature = "std")]
use parking_lot::Mutex;
#[cfg(not(feature = "std"))]
use spin::Mutex;

use super::Field;
use super::ReconstructShard;

const DECODE_MATRIX_CACHE_CAPACITY: usize = 254;
const SHARD_COUNT_ON_STACK: usize = 32;

// /// Parameters for parallelism.
// #[derive(PartialEq, Debug, Clone, Copy)]
// pub struct ParallelParam {
//     /// Number of bytes to split the slices into for computations
//     /// which can be done in parallel.
//     ///
//     /// Default is 32768.
//     pub bytes_per_encode: usize,
// }

// impl ParallelParam {
//     /// Create a new `ParallelParam` with the given split arity.
//     pub fn new(bytes_per_encode: usize) -> ParallelParam {
//         ParallelParam { bytes_per_encode }
//     }
// }

// impl Default for ParallelParam {
//     fn default() -> Self {
//         ParallelParam::new(32768)
//     }
// }

/// Bookkeeper for shard by shard encoding.
///
/// This is useful for avoiding incorrect use of
/// `encode_single` and `encode_single_sep`
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
/// use reed_solomon_erasure::galois_8::Field;
/// let r: ReedSolomon<Field> = ReedSolomon::new(3, 2).unwrap();
///
/// let mut sbs = ShardByShard::new(&r);
///
/// let mut shards = shards!([0u8,  1,  2,  3,  4],
///                          [5,  6,  7,  8,  9],
///                          // say we don't have the 3rd data shard yet
///                          // and we want to fill it in later
///                          [0,  0,  0,  0,  0],
///                          [0,  0,  0,  0,  0],
///                          [0,  0,  0,  0,  0]);
///
/// // encode 1st and 2nd data shard
/// sbs.encode(&mut shards).unwrap();
/// sbs.encode(&mut shards).unwrap();
///
/// // fill in 3rd data shard
/// shards[2][0] = 10.into();
/// shards[2][1] = 11.into();
/// shards[2][2] = 12.into();
/// shards[2][3] = 13.into();
/// shards[2][4] = 14.into();
///
/// // now do the encoding
/// sbs.encode(&mut shards).unwrap();
///
/// assert!(r.verify(&shards).unwrap());
/// # }
/// ```
#[derive(PartialEq, Debug)]
pub struct ShardByShard<'a, F: 'a + Field> {
    codec: &'a ReedSolomon<F>,
    cur_input: usize,
}

impl<'a, F: 'a + Field> ShardByShard<'a, F> {
    /// Creates a new instance of the bookkeeping struct.
    pub fn new(codec: &'a ReedSolomon<F>) -> ShardByShard<'a, F> {
        ShardByShard {
            codec,
            cur_input: 0,
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
    pub fn reset(&mut self) -> Result<(), SBSError> {
        if self.cur_input > 0 && !self.parity_ready() {
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

    fn sbs_encode_checks<U: AsRef<[F::Elem]> + AsMut<[F::Elem]>>(
        &mut self,
        slices: &mut [U],
    ) -> Result<(), SBSError> {
        let internal_checks = |codec: &ReedSolomon<F>, data: &mut [U]| {
            check_piece_count!(all => codec, data);
            check_slices!(multi => data);

            Ok(())
        };

        if self.parity_ready() {
            return Err(SBSError::TooManyCalls);
        }

        match internal_checks(self.codec, slices) {
            Ok(()) => Ok(()),
            Err(e) => Err(SBSError::RSError(e)),
        }
    }

    fn sbs_encode_sep_checks<T: AsRef<[F::Elem]>, U: AsRef<[F::Elem]> + AsMut<[F::Elem]>>(
        &mut self,
        data: &[T],
        parity: &mut [U],
    ) -> Result<(), SBSError> {
        let internal_checks = |codec: &ReedSolomon<F>, data: &[T], parity: &mut [U]| {
            check_piece_count!(data => codec, data);
            check_piece_count!(parity => codec, parity);
            check_slices!(multi => data, multi => parity);

            Ok(())
        };

        if self.parity_ready() {
            return Err(SBSError::TooManyCalls);
        }

        match internal_checks(self.codec, data, parity) {
            Ok(()) => Ok(()),
            Err(e) => Err(SBSError::RSError(e)),
        }
    }

    /// Constructs the parity shards partially using the current input data shard.
    ///
    /// Returns `SBSError::TooManyCalls` when all input data shards
    /// have already been filled in via `encode`
    pub fn encode<T, U>(&mut self, mut shards: T) -> Result<(), SBSError>
    where
        T: AsRef<[U]> + AsMut<[U]>,
        U: AsRef<[F::Elem]> + AsMut<[F::Elem]>,
    {
        let shards = shards.as_mut();
        self.sbs_encode_checks(shards)?;

        self.codec.encode_single(self.cur_input, shards).unwrap();

        self.return_ok_and_incre_cur_input()
    }

    /// Constructs the parity shards partially using the current input data shard.
    ///
    /// Returns `SBSError::TooManyCalls` when all input data shards
    /// have already been filled in via `encode`
    pub fn encode_sep<T: AsRef<[F::Elem]>, U: AsRef<[F::Elem]> + AsMut<[F::Elem]>>(
        &mut self,
        data: &[T],
        parity: &mut [U],
    ) -> Result<(), SBSError> {
        self.sbs_encode_sep_checks(data, parity)?;

        self.codec
            .encode_single_sep(self.cur_input, data[self.cur_input].as_ref(), parity)
            .unwrap();

        self.return_ok_and_incre_cur_input()
    }
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
/// | `encode` | `encode_single` |
/// | `encode_sep` | `encode_single_sep` |
///
/// The `single` variants do similar checks on the provided data shards and parity shards,
/// and also do index check on `i_data`.
///
/// Return `Error::InvalidIndex` if `i_data >= data_shard_count`.
///
/// # Encoding behaviour
/// ## For `encode`
///
/// You do not need to clear the parity shards beforehand, as the methods
/// will overwrite them completely.
///
/// ## For `encode_single`, `encode_single_sep`
///
/// Calling them with `i_data` being `0` will overwrite the parity shards
/// completely. If you are using the methods correctly, then you do not need
/// to clear the parity shards beforehand.
///
/// # Variants of verifying methods
///
/// `verify` allocate sa buffer on the heap of the same size
/// as the parity shards, and encode the input once using the buffer to store
/// the computed parity shards, then check if the provided parity shards
/// match the computed ones.
///
/// `verify_with_buffer`, allows you to provide
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
/// | `verify` | `verify_with_buffer` |
///
/// The `with_buffer` variants also check the dimensions of the buffer and return
/// `Error::TooFewBufferShards`, `Error::TooManyBufferShards`, `Error::EmptyShard`,
/// or `Error::IncorrectShardSize` when applicable.
///
#[derive(Debug)]
pub struct ReedSolomon<F: Field> {
    data_shard_count: usize,
    parity_shard_count: usize,
    total_shard_count: usize,
    /// The matrix of encoder coefficients.
    matrix: Matrix<F>,
    /// Cached decoder coefficient matrices, at most one matrix for each subset of
    /// `total_shard_count` indices of size `data_shard_count`.
    decode_matrix_cache: Mutex<LruCache<Vec<usize>, Arc<Matrix<F>>>>,
}

impl<F: Field> Clone for ReedSolomon<F> {
    fn clone(&self) -> ReedSolomon<F> {
        ReedSolomon::new(self.data_shard_count, self.parity_shard_count)
            .expect("basic checks already passed as precondition of existence of self")
    }
}

impl<F: Field> PartialEq for ReedSolomon<F> {
    fn eq(&self, rhs: &ReedSolomon<F>) -> bool {
        self.data_shard_count == rhs.data_shard_count
            && self.parity_shard_count == rhs.parity_shard_count
    }
}

impl<F: Field> ReedSolomon<F> {
    // AUDIT
    //
    // Error detection responsibilities
    //
    // Terminologies and symbols:
    //   X =A, B, C=> Y: X delegates error checking responsibilities A, B, C to Y
    //   X:= A, B, C: X needs to handle responsibilities A, B, C
    //
    // Encode methods
    //
    // `encode_single`:=
    //   - check index `i_data` within range [0, data shard count)
    //   - check length of `slices` matches total shard count exactly
    //   - check consistency of length of individual slices
    // `encode_single_sep`:=
    //   - check index `i_data` within range [0, data shard count)
    //   - check length of `parity` matches parity shard count exactly
    //   - check consistency of length of individual parity slices
    //   - check length of `single_data` matches length of first parity slice
    // `encode`:=
    //   - check length of `slices` matches total shard count exactly
    //   - check consistency of length of individual slices
    // `encode_sep`:=
    //   - check length of `data` matches data shard count exactly
    //   - check length of `parity` matches parity shard count exactly
    //   - check consistency of length of individual data slices
    //   - check consistency of length of individual parity slices
    //   - check length of first parity slice matches length of first data slice
    //
    // Verify methods
    //
    // `verify`:=
    //   - check length of `slices` matches total shard count exactly
    //   - check consistency of length of individual slices
    //
    //   Generates buffer then passes control to verify_with_buffer
    //
    // `verify_with_buffer`:=
    //   - check length of `slices` matches total shard count exactly
    //   - check length of `buffer` matches parity shard count exactly
    //   - check consistency of length of individual slices
    //   - check consistency of length of individual slices in buffer
    //   - check length of first slice in buffer matches length of first slice
    //
    // Reconstruct methods
    //
    // `reconstruct` =ALL=> `reconstruct_internal`
    // `reconstruct_data`=ALL=> `reconstruct_internal`
    // `reconstruct_internal`:=
    //   - check length of `slices` matches total shard count exactly
    //   - check consistency of length of individual slices
    //   - check length of `slice_present` matches length of `slices`

    fn get_parity_rows(&self) -> SmallVec<[&[F::Elem]; SHARD_COUNT_ON_STACK]> {
        let mut parity_rows = SmallVec::with_capacity(self.parity_shard_count);
        let matrix = &self.matrix;
        for i in self.data_shard_count..self.total_shard_count {
            parity_rows.push(matrix.get_row(i));
        }

        parity_rows
    }

    /// Creates a new instance of Reed-Solomon erasure code encoder/decoder.
    ///
    /// Returns `Error::TooFewDataShards` if `data_shards == 0`.
    ///
    /// Returns `Error::TooFewParityShards` if `parity_shards == 0`.
    ///
    /// Returns `Error::TooManyShards` if `data_shards + parity_shards > F::ORDER`.
    pub fn new(data_shards: usize, parity_shards: usize) -> Result<ReedSolomon<F>, Error> {
        if data_shards == 0 {
            return Err(Error::TooFewDataShards);
        }
        if parity_shards == 0 {
            return Err(Error::TooFewParityShards);
        }
        if data_shards + parity_shards > F::ORDER {
            return Err(Error::TooManyShards);
        }

        let total_shards = data_shards + parity_shards;

        let matrix = Matrix::encode_coeffs(data_shards, total_shards);

        Ok(ReedSolomon {
            data_shard_count: data_shards,
            parity_shard_count: parity_shards,
            total_shard_count: total_shards,
            matrix,
            decode_matrix_cache: Mutex::new(LruCache::new(
                DECODE_MATRIX_CACHE_CAPACITY
                    .try_into()
                    .expect("non-0 constant; qed"),
            )),
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

    fn code_some_slices<T: AsRef<[F::Elem]>, U: AsMut<[F::Elem]>>(
        &self,
        matrix_rows: &[&[F::Elem]],
        inputs: &[T],
        outputs: &mut [U],
    ) {
        for (i, out) in outputs.iter_mut().enumerate() {
            let out = out.as_mut();

            // Initialize the parity row `i`. Do that outside the loop below to avoid a conditional jump.
            let coeff0 = matrix_rows[i][0];
            let inp0 = inputs[0].as_ref();
            F::mul_slice(coeff0, inp0, out);

            // Sum up further codes on the same row `i`.
            for (j, inp) in (1..).zip(inputs[1..].iter()) {
                let coeff = matrix_rows[i][j];
                let inp = inp.as_ref();
                F::mul_slice_add(coeff, inp, out);
            }
        }
    }

    fn code_single_slice<U: AsMut<[F::Elem]>>(
        &self,
        matrix_rows: &[&[F::Elem]],
        i_input: usize,
        input: &[F::Elem],
        outputs: &mut [U],
    ) {
        outputs.iter_mut().enumerate().for_each(|(i_row, output)| {
            let matrix_row_to_use = matrix_rows[i_row][i_input];
            let output = output.as_mut();

            if i_input == 0 {
                F::mul_slice(matrix_row_to_use, input, output);
            } else {
                F::mul_slice_add(matrix_row_to_use, input, output);
            }
        })
    }

    fn check_some_slices_with_buffer<T, U>(
        &self,
        matrix_rows: &[&[F::Elem]],
        inputs: &[T],
        to_check: &[T],
        buffer: &mut [U],
    ) -> bool
    where
        T: AsRef<[F::Elem]>,
        U: AsRef<[F::Elem]> + AsMut<[F::Elem]>,
    {
        self.code_some_slices(matrix_rows, inputs, buffer);

        let at_least_one_mismatch_present = buffer
            .iter_mut()
            .enumerate()
            .map(|(i, expected_parity_shard)| {
                expected_parity_shard.as_ref() == to_check[i].as_ref()
            })
            .any(|x| !x); // find the first false (some slice is different from the expected one)
        !at_least_one_mismatch_present
    }

    /// Constructs the parity shards partially using only the data shard
    /// indexed by `i_data`.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    ///
    /// # Warning
    ///
    /// You must apply this method on the data shards in strict sequential order (0..data shard count),
    /// otherwise the parity shards will be incorrect.
    ///
    /// It is recommended to use the `ShardByShard` bookkeeping struct instead of this method directly.
    pub fn encode_single<T, U>(&self, i_data: usize, mut shards: T) -> Result<(), Error>
    where
        T: AsRef<[U]> + AsMut<[U]>,
        U: AsRef<[F::Elem]> + AsMut<[F::Elem]>,
    {
        let slices = shards.as_mut();

        check_slice_index!(data => self, i_data);
        check_piece_count!(all=> self, slices);
        check_slices!(multi => slices);

        // Get the slice of output buffers.
        let (mut_input, output) = slices.split_at_mut(self.data_shard_count);

        let input = mut_input[i_data].as_ref();

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
    /// You must apply this method on the data shards in strict sequential order (0..data shard count),
    /// otherwise the parity shards will be incorrect.
    ///
    /// It is recommended to use the `ShardByShard` bookkeeping struct instead of this method directly.
    pub fn encode_single_sep<U: AsRef<[F::Elem]> + AsMut<[F::Elem]>>(
        &self,
        i_data: usize,
        single_data: &[F::Elem],
        parity: &mut [U],
    ) -> Result<(), Error> {
        check_slice_index!(data => self, i_data);
        check_piece_count!(parity => self, parity);
        check_slices!(multi => parity, single => single_data);

        let parity_rows = self.get_parity_rows();

        // Do the coding.
        self.code_single_slice(&parity_rows, i_data, single_data, parity);

        Ok(())
    }

    /// Constructs the parity shards.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    pub fn encode<T, U>(&self, mut shards: T) -> Result<(), Error>
    where
        T: AsRef<[U]> + AsMut<[U]>,
        U: AsRef<[F::Elem]> + AsMut<[F::Elem]>,
    {
        let slices: &mut [U] = shards.as_mut();

        check_piece_count!(all => self, slices);
        check_slices!(multi => slices);

        // Get the slice of output buffers.
        let (input, output) = slices.split_at_mut(self.data_shard_count);

        self.encode_sep(&*input, output)
    }

    /// Constructs the parity shards using a read-only view into the
    /// data shards.
    ///
    /// The slots where the parity shards sit at will be overwritten.
    pub fn encode_sep<T: AsRef<[F::Elem]>, U: AsRef<[F::Elem]> + AsMut<[F::Elem]>>(
        &self,
        data: &[T],
        parity: &mut [U],
    ) -> Result<(), Error> {
        check_piece_count!(data => self, data);
        check_piece_count!(parity => self, parity);
        check_slices!(multi => data, multi => parity);

        let parity_rows = self.get_parity_rows();

        // Do the coding.
        self.code_some_slices(&parity_rows, data, parity);

        Ok(())
    }

    /// Checks if the parity shards are correct.
    ///
    /// This is a wrapper of `verify_with_buffer`.
    pub fn verify<T: AsRef<[F::Elem]>>(&self, slices: &[T]) -> Result<bool, Error> {
        check_piece_count!(all => self, slices);
        check_slices!(multi => slices);

        let slice_len = slices[0].as_ref().len();

        let mut buffer: SmallVec<[Vec<F::Elem>; SHARD_COUNT_ON_STACK]> =
            SmallVec::with_capacity(self.parity_shard_count);

        for _ in 0..self.parity_shard_count {
            buffer.push(vec![F::zero(); slice_len]);
        }

        self.verify_with_buffer(slices, &mut buffer)
    }

    /// Checks if the parity shards are correct.
    pub fn verify_with_buffer<T, U>(&self, slices: &[T], buffer: &mut [U]) -> Result<bool, Error>
    where
        T: AsRef<[F::Elem]>,
        U: AsRef<[F::Elem]> + AsMut<[F::Elem]>,
    {
        check_piece_count!(all => self, slices);
        check_piece_count!(parity_buf => self, buffer);
        check_slices!(multi => slices, multi => buffer);

        let data = &slices[0..self.data_shard_count];
        let to_check = &slices[self.data_shard_count..];

        let parity_rows = self.get_parity_rows();

        Ok(self.check_some_slices_with_buffer(&parity_rows, data, to_check, buffer))
    }

    /// Reconstructs all shards.
    ///
    /// The shards marked not present are only overwritten when no error
    /// is detected. All provided shards must have the same length.
    ///
    /// This means if the method returns an `Error`, then nothing is touched.
    ///
    /// `reconstruct`, `reconstruct_data`, `reconstruct_shards`,
    /// `reconstruct_data_shards` share the same core code base.
    pub fn reconstruct<T: ReconstructShard<F>>(&self, slices: &mut [T]) -> Result<(), Error> {
        self.reconstruct_internal(slices, false)
    }

    /// Reconstructs only the data shards.
    ///
    /// The shards marked not present are only overwritten when no error
    /// is detected. All provided shards must have the same length.
    ///
    /// This means if the method returns an `Error`, then nothing is touched.
    ///
    /// `reconstruct`, `reconstruct_data`, `reconstruct_shards`,
    /// `reconstruct_data_shards` share the same core code base.
    pub fn reconstruct_data<T: ReconstructShard<F>>(&self, slices: &mut [T]) -> Result<(), Error> {
        self.reconstruct_internal(slices, true)
    }

    /// Compute or fetch from cache the decoding coefficients for the missing shards.
    ///
    /// - `valid_indices` : length `self.data_shard_count`, the field element for each available shard.
    /// - `missing_indices`: length `self.parity_shard_count`, the field element at which we want to
    ///   evaluate/interpolate (each missing shard position). `missing_indices` must be the complement of
    ///   `valid_indices` relative to the row indices of `self.matrix`.
    ///
    /// Returns an `self.data_shard_count` by `self.parity_shard_count` matrix with multiplicative
    /// coefficients to decode the missing rows.
    ///
    /// The function splits the systematic generator matrix $G = [I; E]$ (where $E$ is the encode
    /// coefficient matrix and $I$ is the identity matrix of suitable size) row-wise into $G_v$ and
    /// $G_m$ - the coefficient rows that encode available and missing shards respectively. It
    /// returns the the reconstruction coefficients as $C = G_m \cdot G_v^{-1}$.
    ///
    /// $C$ is stored in the cache at the key `missing_indices`. This key is sufficient because it
    /// implies the other dimension of $C$, that is `valid_indices`.
    fn get_decode_matrix(
        &self,
        valid_indices: &[usize],
        missing_indices: &[usize],
    ) -> Arc<Matrix<F>> {
        {
            let mut cache = self.decode_matrix_cache.lock();
            if let Some(entry) = cache.get(missing_indices) {
                return entry.clone();
            }
        }

        let encode_coeffs_rows = self.matrix.rows();

        let valid_rows = valid_indices
            .iter()
            .map(|i| encode_coeffs_rows[*i])
            .collect();
        let gv = Matrix::from_rows(valid_rows);
        let gv_inv = gv.invert().unwrap();

        let missing_rows = missing_indices
            .iter()
            .map(|i| encode_coeffs_rows[*i])
            .collect();
        let gm = Matrix::from_rows(missing_rows);

        let decode_coeffs = Arc::new(gm.multiply(&gv_inv));
        {
            let decode_coeffs = decode_coeffs.clone();
            let mut cache = self.decode_matrix_cache.lock();
            cache.put(Vec::from(missing_indices), decode_coeffs);
        }
        decode_coeffs
    }

    fn reconstruct_internal<T: ReconstructShard<F>>(
        &self,
        shards: &mut [T],
        data_only: bool,
    ) -> Result<(), Error> {
        check_piece_count!(all => self, shards);

        // Quick check: are all of the shards present?  If so, there's
        // nothing to do.
        let mut number_present = 0;
        let mut shard_len = None;

        for shard in shards.iter_mut() {
            if let Some(len) = shard.len() {
                if len == 0 {
                    return Err(Error::EmptyShard);
                }
                number_present += 1;
                if let Some(old_len) = shard_len {
                    if len != old_len {
                        // mismatch between shards.
                        return Err(Error::IncorrectShardSize);
                    }
                }
                shard_len = Some(len);
            }
        }

        if number_present == self.total_shard_count {
            // Cool.  All of the shards are there.  We don't
            // need to do anything.
            return Ok(());
        }

        // More complete sanity check
        if number_present < self.data_shard_count {
            return Err(Error::TooFewShardsPresent);
        }

        let shard_len = shard_len.expect("at least one shard present; qed");

        // Pull out an array holding just the shards that
        // correspond to the rows of the submatrix.  These shards
        // will be the input to the decoding process that re-creates
        // the missing data shards.
        //
        // Also, create an array of indices of the valid rows we do have
        // and the missing rows.
        //
        // The missing indices are used to construct the data decode matrix,
        // and as key in the data decode matrix cache.
        //
        // We need exactly N valid indices, where N = `data_shard_count`,
        // as the data decode matrix is a N x N matrix, thus needs
        // N valid indices for determining the N rows to pick from
        // `self.matrix`.
        let mut valid_shards: SmallVec<[&[F::Elem]; SHARD_COUNT_ON_STACK]> =
            SmallVec::with_capacity(self.data_shard_count);
        let mut valid_indices: SmallVec<[usize; SHARD_COUNT_ON_STACK]> =
            SmallVec::with_capacity(self.data_shard_count);
        // the complement of valid_indices
        let mut missing_indices: SmallVec<[usize; SHARD_COUNT_ON_STACK]> =
            SmallVec::with_capacity(self.parity_shard_count);
        // content of the output shards
        let mut reconstruct_shards: SmallVec<[&mut [F::Elem]; SHARD_COUNT_ON_STACK]> =
            SmallVec::with_capacity(self.parity_shard_count);
        // reconstruct_indices indexes missing_indices
        let mut reconstruct_indices: SmallVec<[usize; SHARD_COUNT_ON_STACK]> =
            SmallVec::with_capacity(self.parity_shard_count);

        for (shard_idx, shard) in shards.iter_mut().enumerate() {
            let shard_data = if data_only && shard_idx >= self.data_shard_count {
                shard.get().ok_or(None)
            } else {
                shard.get_or_initialize(shard_len).map_err(Some)
            };

            match shard_data {
                Ok(shard) => {
                    if valid_shards.len() < self.data_shard_count {
                        valid_shards.push(shard);
                        valid_indices.push(shard_idx);
                    } else {
                        missing_indices.push(shard_idx);
                    }
                }
                Err(None) => {
                    // the shard data is not meant to be initialized here,
                    // but we should still note it missing.
                    missing_indices.push(shard_idx);
                }
                Err(Some(x)) => {
                    let shard = x?;
                    if !data_only || shard_idx < self.data_shard_count {
                        reconstruct_shards.push(shard);
                        // Reconstruction shard indices are relative to the missing indices array.
                        reconstruct_indices.push(missing_indices.len());
                    }
                    missing_indices.push(shard_idx);
                }
            }
        }

        let decode_matrix = self.get_decode_matrix(&valid_indices, &missing_indices);

        // Decode coefficient matrix to recover the desired shards, data and parity alike
        let reconstruct_decode_rows: SmallVec<[&[F::Elem]; SHARD_COUNT_ON_STACK]> =
            reconstruct_indices
                .iter()
                .map(|i| decode_matrix.get_row(*i))
                .collect();

        self.code_some_slices(
            &reconstruct_decode_rows,
            &valid_shards,
            &mut reconstruct_shards,
        );

        Ok(())
    }
}
