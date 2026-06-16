extern crate alloc;

use alloc::vec;
use alloc::vec::Vec;

use hashlink::LruCache;
#[cfg(feature = "std")]
use parking_lot::Mutex;
use smallvec::SmallVec;
#[cfg(not(feature = "std"))]
use spin::Mutex;

use crate::Field;
use crate::errors::Error;
use crate::matrix::Matrix;

use super::{
    CodecFamily, CodecOptions, DATA_DECODE_MATRIX_CACHE_MAX_CAPACITY,
    DATA_DECODE_MATRIX_CACHE_MIN_CAPACITY, MatrixMode, ReedSolomon,
};
#[cfg(feature = "std")]
use super::{
    ReconstructionCacheMetrics, ReconstructionCacheStats, RuntimeProfileMetrics,
    RuntimeProfileStats,
};

impl<F: Field> ReedSolomon<F> {
    pub(crate) fn normalize_inversion_cache_capacity(
        data_shards: usize,
        parity_shards: usize,
        requested_capacity: usize,
    ) -> usize {
        if requested_capacity > 0 {
            return requested_capacity;
        }

        Self::recommended_inversion_cache_capacity(data_shards, parity_shards)
    }

    pub(crate) fn derive_inversion_cache_capacity(
        data_shards: usize,
        parity_shards: usize,
    ) -> usize {
        let total_shards = data_shards.saturating_add(parity_shards);
        let heuristic = total_shards
            .saturating_mul(parity_shards.max(1))
            .saturating_mul(2);
        let rounded = heuristic
            .checked_next_power_of_two()
            .unwrap_or(DATA_DECODE_MATRIX_CACHE_MAX_CAPACITY);

        rounded.clamp(
            DATA_DECODE_MATRIX_CACHE_MIN_CAPACITY,
            DATA_DECODE_MATRIX_CACHE_MAX_CAPACITY,
        )
    }

    pub(crate) fn get_parity_rows(&self) -> SmallVec<[&[F::Elem]; 32]> {
        let mut parity_rows = SmallVec::with_capacity(self.parity_shard_count);
        let matrix = &self.matrix;
        for i in self.data_shard_count..self.total_shard_count {
            parity_rows.push(matrix.get_row(i));
        }

        parity_rows
    }

    pub(crate) fn build_matrix(data_shards: usize, total_shards: usize) -> Result<Matrix<F>, Error> {
        let vandermonde = Matrix::vandermonde(total_shards, data_shards);

        let top = vandermonde.sub_matrix(0, 0, data_shards, data_shards);
        let top_inverted = top.invert().map_err(|_| {
            Error::InvalidCustomMatrix
        })?;

        vandermonde.multiply(&top_inverted).map_err(|_| Error::InvalidCustomMatrix)
    }

    pub(crate) fn build_cauchy_matrix(
        data_shards: usize,
        total_shards: usize,
    ) -> Result<Matrix<F>, Error> {
        let mut result = Matrix::new(total_shards, data_shards);

        for r in 0..total_shards {
            if r < data_shards {
                result.set(r, r, F::one());
            } else {
                for c in 0..data_shards {
                    let denominator = F::add(F::nth(r), F::nth(c));
                    if denominator == F::zero() {
                        return Err(Error::InvalidCustomMatrix);
                    }
                    result.set(r, c, F::div(F::one(), denominator));
                }
            }
        }

        Ok(result)
    }

    pub(crate) fn build_jerasure_like_matrix(
        data_shards: usize,
        total_shards: usize,
    ) -> Result<Matrix<F>, Error> {
        let mut vm = Matrix::vandermonde(total_shards, data_shards);

        vm.set(0, 0, F::one());
        for i in 1..data_shards {
            vm.set(0, i, F::zero());
        }

        for i in 0..data_shards.saturating_sub(1) {
            vm.set(total_shards - 1, i, F::zero());
        }
        vm.set(total_shards - 1, data_shards - 1, F::one());

        for i in 0..data_shards {
            let mut r = i;
            while r < total_shards && vm.get(r, i) == F::zero() {
                r += 1;
            }
            if r != i {
                vm.swap_rows(r, i);
            } else if vm.get(i, i) == F::zero() {
                return Err(Error::InvalidCustomMatrix);
            }

            let scale = match vm.get(i, i) {
                diagonal if diagonal == F::zero() => {
                    return Err(Error::InvalidCustomMatrix);
                }
                diagonal if diagonal != F::one() => F::div(F::one(), diagonal),
                _ => F::one(),
            };
            if scale != F::one() {
                for row in 0..total_shards {
                    vm.set(row, i, F::mul(vm.get(row, i), scale));
                }
            }

            for j in 0..data_shards {
                let value = vm.get(i, j);
                if j != i && value != F::zero() {
                    for row in 0..total_shards {
                        vm.set(
                            row,
                            j,
                            F::add(vm.get(row, j), F::mul(value, vm.get(row, i))),
                        );
                    }
                }
            }
        }

        for j in 0..data_shards {
            let value = vm.get(data_shards, j);
            if value == F::zero() {
                return Err(Error::InvalidCustomMatrix);
            }

            if value != F::one() {
                let scale = F::div(F::one(), value);
                for row in data_shards..total_shards {
                    vm.set(row, j, F::mul(vm.get(row, j), scale));
                }
            }
        }

        for row in (data_shards + 1)..total_shards {
            let value = vm.get(row, 0);
            if value == F::zero() {
                return Err(Error::InvalidCustomMatrix);
            }

            if value != F::one() {
                let scale = F::div(F::one(), value);
                for col in 0..data_shards {
                    vm.set(row, col, F::mul(vm.get(row, col), scale));
                }
            }
        }

        Ok(vm)
    }

    pub(crate) fn build_custom_matrix(
        data_shards: usize,
        total_shards: usize,
        custom_matrix: &[Vec<F::Elem>],
    ) -> Result<Matrix<F>, Error> {
        let parity_shards = total_shards.saturating_sub(data_shards);
        if custom_matrix.len() < parity_shards {
            return Err(Error::InvalidCustomMatrix);
        }
        if custom_matrix
            .iter()
            .take(parity_shards)
            .any(|row| row.len() < data_shards)
        {
            return Err(Error::InvalidCustomMatrix);
        }

        let mut result = Matrix::new(total_shards, data_shards);
        for row in 0..data_shards {
            result.set(row, row, F::one());
        }
        for (offset, row) in custom_matrix.iter().take(parity_shards).enumerate() {
            for (col, value) in row.iter().take(data_shards).enumerate() {
                result.set(data_shards + offset, col, *value);
            }
        }

        Ok(result)
    }

    pub(crate) fn build_matrix_with_options(
        data_shards: usize,
        total_shards: usize,
        options: CodecOptions,
    ) -> Result<Matrix<F>, Error> {
        if options.codec_family != CodecFamily::Classic {
            return Err(Error::UnsupportedCodecFamily);
        }

        match options.matrix_mode {
            MatrixMode::Vandermonde => Self::build_matrix(data_shards, total_shards),
            MatrixMode::Cauchy => Self::build_cauchy_matrix(data_shards, total_shards),
            MatrixMode::JerasureLike => {
                Self::build_jerasure_like_matrix(data_shards, total_shards)
            }
            MatrixMode::Custom => Err(Error::InvalidCustomMatrix),
        }
    }

    /// Create a new codec with default options.
    ///
    /// Returns [`Error::TooFewDataShards`] or [`Error::TooFewParityShards`] if
    /// either count is zero, or [`Error::TooManyShards`] if the total exceeds the field order.
    pub fn new(data_shards: usize, parity_shards: usize) -> Result<ReedSolomon<F>, Error> {
        Self::with_options(data_shards, parity_shards, CodecOptions::default())
    }

    /// Create a new codec with explicit [`CodecOptions`].
    pub fn with_options(
        data_shards: usize,
        parity_shards: usize,
        mut options: CodecOptions,
    ) -> Result<ReedSolomon<F>, Error> {
        if data_shards == 0 {
            return Err(Error::TooFewDataShards);
        }
        if parity_shards == 0 {
            return Err(Error::TooFewParityShards);
        }
        if data_shards + parity_shards > F::ORDER {
            return Err(Error::TooManyShards);
        }

        super::leopard::validate_leopard_family::<F>(
            options.codec_family,
            data_shards,
            parity_shards,
        )?;

        let total_shards = data_shards + parity_shards;

        options.inversion_cache_capacity = Self::normalize_inversion_cache_capacity(
            data_shards,
            parity_shards,
            options.inversion_cache_capacity,
        );

        let matrix = match options.codec_family {
            CodecFamily::Classic => {
                Self::build_matrix_with_options(data_shards, total_shards, options)?
            }
            CodecFamily::LeopardGF8 | CodecFamily::LeopardGF16 => {
                Self::build_matrix(data_shards, total_shards)?
            }
        };
        let family_state = super::leopard::build_family_state(
            options.codec_family,
            data_shards,
            parity_shards,
            &matrix,
        )?;
        #[cfg(feature = "std")]
        let policy_cache = Self::resolve_policy_cache_with_options(options);

        Ok(ReedSolomon {
            data_shard_count: data_shards,
            parity_shard_count: parity_shards,
            total_shard_count: total_shards,
            codec_family: options.codec_family,
            family_state,
            matrix,
            options,
            #[cfg(feature = "std")]
            policy_cache,
            data_decode_matrix_cache: Mutex::new(LruCache::new(options.inversion_cache_capacity)),
            #[cfg(feature = "std")]
            reconstruction_cache_metrics: ReconstructionCacheMetrics::default(),
            #[cfg(feature = "std")]
            runtime_profile_metrics: RuntimeProfileMetrics::default(),
        })
    }

    /// Returns the number of data shards.
    pub fn data_shard_count(&self) -> usize {
        self.data_shard_count
    }

    /// Returns the number of parity shards.
    pub fn parity_shard_count(&self) -> usize {
        self.parity_shard_count
    }

    /// Returns the total number of shards (data + parity).
    pub fn total_shard_count(&self) -> usize {
        self.total_shard_count
    }

    /// Returns the codec family (Classic, LeopardGF8, or LeopardGF16).
    pub fn codec_family(&self) -> CodecFamily {
        self.codec_family
    }

    /// Returns the Leopard GF8 setup matrix shape `(rows, cols)`, or `None` if not using LeopardGF8.
    pub fn leopard_setup_matrix_shape(&self) -> Option<(usize, usize)> {
        let codec = super::leopard::leopard_gf8_state(&self.family_state).ok()?;
        Some(codec.setup_shape())
    }

    /// Returns the inversion cache capacity.
    pub fn inversion_cache_capacity(&self) -> usize {
        self.options.inversion_cache_capacity
    }

    /// Returns the recommended inversion cache capacity for the given shard counts.
    pub fn recommended_inversion_cache_capacity(data_shards: usize, parity_shards: usize) -> usize {
        Self::derive_inversion_cache_capacity(data_shards, parity_shards)
    }

    /// Returns a snapshot of reconstruction cache hit/miss statistics.
    #[cfg(feature = "std")]
    pub fn reconstruction_cache_stats(&self) -> ReconstructionCacheStats {
        self.reconstruction_cache_metrics.snapshot()
    }

    /// Returns a snapshot of runtime profiling metrics.
    #[cfg(feature = "std")]
    pub fn runtime_profile_stats(&self) -> RuntimeProfileStats {
        self.runtime_profile_metrics.snapshot()
    }

    /// Resets all runtime profiling counters to zero.
    #[cfg(feature = "std")]
    pub fn reset_runtime_profile_stats(&self) {
        self.runtime_profile_metrics.reset();
    }

    #[cfg(feature = "std")]
    pub(crate) fn record_reconstruct_entry_path(&self, parallel: bool) {
        self.runtime_profile_metrics
            .record_reconstruct_entry(parallel);
    }

    #[cfg(feature = "std")]
    pub(crate) fn record_reconstruct_opt_fallback_serial_path(&self) {
        self.runtime_profile_metrics
            .record_reconstruct_opt_fallback_serial();
    }

    #[cfg(feature = "std")]
    pub(crate) fn record_reconstruct_runtime(
        &self,
        data_only: bool,
        missing_data_count: usize,
        missing_parity_count: usize,
        all_present: bool,
    ) {
        self.runtime_profile_metrics.record_reconstruct(
            data_only,
            missing_data_count,
            missing_parity_count,
            all_present,
        );
    }

    #[cfg(feature = "std")]
    pub(crate) fn record_reconstruct_data_stage_runtime(
        &self,
        shard_len: usize,
        output_count: usize,
    ) {
        self.runtime_profile_metrics
            .record_reconstruct_data_stage(shard_len, output_count);
    }

    #[cfg(feature = "std")]
    pub(crate) fn record_reconstruct_parity_stage_runtime(
        &self,
        shard_len: usize,
        output_count: usize,
    ) {
        self.runtime_profile_metrics
            .record_reconstruct_parity_stage(shard_len, output_count);
    }

    /// Split a contiguous data buffer into `data_shard_count` equal-length shards.
    ///
    /// The last shard is zero-padded if `data.len()` is not evenly divisible.
    pub fn split(&self, data: &[F::Elem]) -> Result<Vec<Vec<F::Elem>>, Error> {
        let data_shards = self.data_shard_count;
        let shard_len = if data.is_empty() {
            0
        } else {
            data.len().div_ceil(data_shards)
        };

        let mut shards = Vec::with_capacity(data_shards);
        for i in 0..data_shards {
            let start = i * shard_len;
            let end = core::cmp::min(start + shard_len, data.len());
            let mut shard = vec![F::zero(); shard_len];
            if start < data.len() {
                shard[..end - start].copy_from_slice(&data[start..end]);
            }
            shards.push(shard);
        }

        Ok(shards)
    }

    /// Join data shards back into a single contiguous buffer.
    ///
    /// Truncates to `out_len` bytes. Requires exactly `data_shard_count` shards.
    pub fn join<T: AsRef<[F::Elem]>>(
        &self,
        shards: &[T],
        out_len: usize,
    ) -> Result<Vec<F::Elem>, Error> {
        check_piece_count!(data => self, shards);
        check_slices!(multi => shards);

        let available = shards
            .iter()
            .map(|shard| shard.as_ref().len())
            .sum::<usize>();
        let target_len = core::cmp::min(out_len, available);
        let mut result = Vec::with_capacity(target_len);

        for shard in shards {
            let remaining = target_len.saturating_sub(result.len());
            if remaining == 0 {
                break;
            }

            let data = shard.as_ref();
            let to_take = core::cmp::min(remaining, data.len());
            result.extend_from_slice(&data[..to_take]);
        }

        result.truncate(target_len);
        Ok(result)
    }

    /// Create a codec with a user-provided encoding matrix.
    pub fn with_custom_matrix(
        data_shards: usize,
        parity_shards: usize,
        custom_matrix: &[Vec<F::Elem>],
        mut options: CodecOptions,
    ) -> Result<ReedSolomon<F>, Error> {
        if data_shards == 0 {
            return Err(Error::TooFewDataShards);
        }
        if parity_shards == 0 {
            return Err(Error::TooFewParityShards);
        }
        if data_shards + parity_shards > F::ORDER {
            return Err(Error::TooManyShards);
        }

        super::leopard::validate_leopard_family::<F>(
            options.codec_family,
            data_shards,
            parity_shards,
        )?;

        let total_shards = data_shards + parity_shards;
        options.matrix_mode = MatrixMode::Custom;
        options.inversion_cache_capacity = Self::normalize_inversion_cache_capacity(
            data_shards,
            parity_shards,
            options.inversion_cache_capacity,
        );

        if options.codec_family != CodecFamily::Classic {
            return Err(Error::UnsupportedCodecFamily);
        }

        let matrix = Self::build_custom_matrix(data_shards, total_shards, custom_matrix)?;
        let family_state = super::leopard::build_family_state(
            options.codec_family,
            data_shards,
            parity_shards,
            &matrix,
        )?;
        #[cfg(feature = "std")]
        let policy_cache = Self::resolve_policy_cache_with_options(options);

        Ok(ReedSolomon {
            data_shard_count: data_shards,
            parity_shard_count: parity_shards,
            total_shard_count: total_shards,
            codec_family: options.codec_family,
            family_state,
            matrix,
            options,
            #[cfg(feature = "std")]
            policy_cache,
            data_decode_matrix_cache: Mutex::new(LruCache::new(options.inversion_cache_capacity)),
            #[cfg(feature = "std")]
            reconstruction_cache_metrics: ReconstructionCacheMetrics::default(),
            #[cfg(feature = "std")]
            runtime_profile_metrics: RuntimeProfileMetrics::default(),
        })
    }
}
