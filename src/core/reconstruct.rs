extern crate alloc;

use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;

use smallvec::SmallVec;

use crate::errors::Error;
use crate::{Field, ReconstructShard};

#[cfg(feature = "std")]
use rayon::prelude::*;
#[cfg(feature = "std")]
use std::sync::atomic::Ordering;

#[cfg(not(feature = "std"))]
use super::ReedSolomon;
#[cfg(feature = "std")]
use super::{ParallelPolicy, ReedSolomon};

impl<F: Field> ReedSolomon<F> {
    #[cfg(feature = "std")]
    pub(crate) fn code_some_slices_par_raw(
        &self,
        matrix_rows: &[&[F::Elem]],
        inputs: &[&[F::Elem]],
        outputs: &mut [&mut [F::Elem]],
    ) where
        F::Elem: Send + Sync,
    {
        let shard_len = inputs.first().map(|input| input.len()).unwrap_or(0);
        if shard_len == 0 {
            return;
        }

        if outputs.len() <= 2 {
            self.code_some_slices_one_or_two_outputs_reconstruct_data_par_raw(
                matrix_rows,
                inputs,
                outputs,
            );
            return;
        }

        let decision = self.parallel_policy(shard_len, outputs.len());
        if !decision.use_parallel {
            self.code_some_slices_chunked(matrix_rows, inputs, outputs);
            return;
        }
        self.code_some_slices_par_chunked(matrix_rows, inputs, outputs, decision.chunk_len);
    }

    #[cfg(feature = "std")]
    pub(crate) fn code_some_slices_with_policy_raw(
        &self,
        matrix_rows: &[&[F::Elem]],
        inputs: &[&[F::Elem]],
        outputs: &mut [&mut [F::Elem]],
        policy: ParallelPolicy,
    ) where
        F::Elem: Send + Sync,
    {
        let shard_len = inputs.first().map(|input| input.len()).unwrap_or(0);
        if shard_len == 0 {
            return;
        }

        if outputs.len() <= 2 {
            self.code_some_slices_one_or_two_outputs_reconstruct_data_par_raw(
                matrix_rows,
                inputs,
                outputs,
            );
            return;
        }

        let decision = policy.decide(
            shard_len,
            self.data_shard_count,
            outputs.len(),
            std::thread::available_parallelism()
                .map(|parallelism| parallelism.get())
                .unwrap_or(1),
        );
        self.runtime_profile_metrics
            .record_parallel_policy(decision);
        if !decision.use_parallel {
            self.code_some_slices_chunked(matrix_rows, inputs, outputs);
            return;
        }
        self.code_some_slices_par_chunked(matrix_rows, inputs, outputs, decision.chunk_len);
    }

    #[cfg(feature = "std")]
    pub(crate) fn code_some_slices_two_outputs_reconstruct_data_par_raw(
        &self,
        matrix_rows: &[&[F::Elem]],
        inputs: &[&[F::Elem]],
        outputs: &mut [&mut [F::Elem]],
    ) where
        F::Elem: Send + Sync,
    {
        debug_assert_eq!(2, matrix_rows.len());
        debug_assert_eq!(2, outputs.len());

        let shard_len = inputs.first().map(|input| input.len()).unwrap_or(0);
        if shard_len == 0 {
            return;
        }

        let decision = self.parallel_policy(shard_len, outputs.len());
        self.runtime_profile_metrics
            .record_parallel_policy(decision);
        if !decision.use_parallel {
            self.code_some_slices_chunked(matrix_rows, inputs, outputs);
            return;
        }

        let chunk_len = if self.data_shard_count <= 16 {
            core::cmp::min(shard_len, core::cmp::max(decision.chunk_len, 512 * 1024))
        } else {
            decision.chunk_len
        };
        self.runtime_profile_metrics.record_code_some(
            true,
            shard_len,
            inputs.len(),
            outputs.len(),
            chunk_len,
        );

        let data_shard_count = self.data_shard_count;
        outputs
            .par_iter_mut()
            .enumerate()
            .for_each(|(i_row, output)| {
                let matrix_row = matrix_rows[i_row];

                let mut start = 0;
                while start < shard_len {
                    let end = core::cmp::min(start + chunk_len, shard_len);
                    let output_chunk = &mut output[start..end];

                    F::mul_slice(matrix_row[0], &inputs[0][start..end], output_chunk);
                    for i_input in 1..data_shard_count {
                        F::mul_slice_add(
                            matrix_row[i_input],
                            &inputs[i_input][start..end],
                            output_chunk,
                        );
                    }

                    start = end;
                }
            });
    }

    #[cfg(feature = "std")]
    pub(crate) fn code_some_slices_one_or_two_outputs_reconstruct_data_par_raw(
        &self,
        matrix_rows: &[&[F::Elem]],
        inputs: &[&[F::Elem]],
        outputs: &mut [&mut [F::Elem]],
    ) where
        F::Elem: Send + Sync,
    {
        debug_assert!((1..=2).contains(&outputs.len()));

        if outputs.len() == 1 {
            let shard_len = inputs.first().map(|input| input.len()).unwrap_or(0);
            if shard_len == 0 {
                return;
            }

            let decision = self.parallel_policy(shard_len, outputs.len());
            self.runtime_profile_metrics
                .record_parallel_policy(decision);
            if !decision.use_parallel {
                self.code_some_slices_chunked(matrix_rows, inputs, outputs);
                return;
            }

            self.runtime_profile_metrics.record_code_some(
                true,
                shard_len,
                inputs.len(),
                outputs.len(),
                decision.chunk_len,
            );

            let data_shard_count = self.data_shard_count;
            let matrix_row = matrix_rows[0];
            outputs[0]
                .par_chunks_mut(decision.chunk_len)
                .enumerate()
                .for_each(|(chunk_idx, output_chunk)| {
                    let start = chunk_idx * decision.chunk_len;
                    let end = start + output_chunk.len();

                    F::mul_slice(matrix_row[0], &inputs[0][start..end], output_chunk);
                    for i_input in 1..data_shard_count {
                        F::mul_slice_add(
                            matrix_row[i_input],
                            &inputs[i_input][start..end],
                            output_chunk,
                        );
                    }
                });
            return;
        }

        self.code_some_slices_two_outputs_reconstruct_data_par_raw(matrix_rows, inputs, outputs);
    }

    #[cfg(feature = "std")]
    pub(crate) fn reconstruct_internal_option_vec_par(
        &self,
        shards: &mut [Option<Vec<F::Elem>>],
        data_only: bool,
    ) -> Result<(), Error>
    where
        F::Elem: Send + Sync,
    {
        let (data_policy, parity_policy) = self.policy_cache.reconstruct_stage_policies(data_only);
        self.reconstruct_internal_option_vec_par_with_stage_policies(
            shards,
            data_only,
            data_policy,
            parity_policy,
        )
    }

    #[cfg(feature = "std")]
    pub(crate) fn reconstruct_internal_option_vec_par_with_stage_policies(
        &self,
        shards: &mut [Option<Vec<F::Elem>>],
        data_only: bool,
        data_policy: ParallelPolicy,
        parity_policy: ParallelPolicy,
    ) -> Result<(), Error>
    where
        F::Elem: Send + Sync,
    {
        check_piece_count!(all => self, shards);

        let data_shard_count = self.data_shard_count;

        let mut number_present = 0;
        let mut shard_len = None;
        for shard in shards.iter() {
            if let Some(shard) = shard.as_ref() {
                let len = shard.len();
                if len == 0 {
                    return Err(Error::EmptyShard);
                }
                number_present += 1;
                if let Some(old_len) = shard_len
                    && len != old_len
                {
                    return Err(Error::IncorrectShardSize);
                }
                shard_len = Some(len);
            }
        }

        if number_present == self.total_shard_count {
            self.runtime_profile_metrics
                .record_reconstruct(data_only, 0, 0, true);
            return Ok(());
        }
        if number_present < data_shard_count {
            return Err(Error::TooFewShardsPresent);
        }

        let shard_len = shard_len.expect("at least one shard present; qed");

        let mut valid_indices: SmallVec<[usize; 32]> = SmallVec::with_capacity(data_shard_count);
        let mut invalid_indices: SmallVec<[usize; 32]> =
            SmallVec::with_capacity(self.total_shard_count);
        let mut missing_data_indices: SmallVec<[usize; 32]> = SmallVec::new();
        let mut missing_parity_indices: SmallVec<[usize; 32]> = SmallVec::new();

        for (matrix_row, shard) in shards.iter().enumerate() {
            match shard.as_ref() {
                Some(_shard) => {
                    if valid_indices.len() < data_shard_count {
                        valid_indices.push(matrix_row);
                    }
                }
                None => {
                    invalid_indices.push(matrix_row);
                    if matrix_row < data_shard_count {
                        missing_data_indices.push(matrix_row);
                    } else if !data_only {
                        missing_parity_indices.push(matrix_row);
                    }
                }
            }
        }

        self.runtime_profile_metrics.record_reconstruct(
            data_only,
            missing_data_indices.len(),
            missing_parity_indices.len(),
            false,
        );

        let data_decode_matrix = self
            .get_data_decode_matrix(&valid_indices, &invalid_indices)?;

        if !missing_data_indices.is_empty() {
            #[cfg(feature = "std")]
            self.runtime_profile_metrics
                .record_reconstruct_data_stage(shard_len, missing_data_indices.len());
            let mut matrix_rows: SmallVec<[&[F::Elem]; 32]> =
                SmallVec::with_capacity(missing_data_indices.len());
            for &idx in &missing_data_indices {
                matrix_rows.push(data_decode_matrix.get_row(idx));
            }

            let mut recovered_data: Vec<Vec<F::Elem>> = missing_data_indices
                .iter()
                .map(|_| vec![F::zero(); shard_len])
                .collect();

            {
                let mut sub_shards: SmallVec<[&[F::Elem]; 32]> =
                    SmallVec::with_capacity(data_shard_count);
                for &idx in &valid_indices {
                    let shard = shards[idx].as_deref().ok_or(Error::TooFewShardsPresent)?;
                    sub_shards.push(shard);
                }

                let mut outputs: SmallVec<[&mut [F::Elem]; 32]> = recovered_data
                    .iter_mut()
                    .map(|shard| shard.as_mut_slice())
                    .collect();

                if data_only && outputs.len() <= 2 {
                    self.runtime_profile_metrics
                        .record_reconstruct_data_small_output_specialized();
                    self.code_some_slices_one_or_two_outputs_reconstruct_data_par_raw(
                        &matrix_rows,
                        &sub_shards,
                        &mut outputs,
                    );
                } else {
                    self.code_some_slices_with_policy_raw(
                        &matrix_rows,
                        &sub_shards,
                        &mut outputs,
                        data_policy,
                    );
                }
            }

            for (idx, recovered) in missing_data_indices.into_iter().zip(recovered_data) {
                shards[idx] = Some(recovered);
            }
        }

        if data_only {
            return Ok(());
        }

        if missing_parity_indices.is_empty() {
            return Ok(());
        }

        #[cfg(feature = "std")]
        self.runtime_profile_metrics
            .record_reconstruct_parity_stage(shard_len, missing_parity_indices.len());
        let parity_rows = self.get_parity_rows();
        let mut matrix_rows: SmallVec<[&[F::Elem]; 32]> =
            SmallVec::with_capacity(missing_parity_indices.len());
        for &idx in &missing_parity_indices {
            matrix_rows.push(parity_rows[idx - data_shard_count]);
        }

        let mut recovered_parity: Vec<Vec<F::Elem>> = missing_parity_indices
            .iter()
            .map(|_| vec![F::zero(); shard_len])
            .collect();

        {
            let mut all_data: SmallVec<[&[F::Elem]; 32]> =
                SmallVec::with_capacity(data_shard_count);
            for shard in shards.iter().take(data_shard_count) {
                let shard = shard.as_deref().ok_or(Error::TooFewShardsPresent)?;
                all_data.push(shard);
            }

            let mut outputs: SmallVec<[&mut [F::Elem]; 32]> = recovered_parity
                .iter_mut()
                .map(|shard| shard.as_mut_slice())
                .collect();

            self.code_some_slices_with_policy_raw(
                &matrix_rows,
                &all_data,
                &mut outputs,
                parity_policy,
            );
        }
        for (idx, recovered) in missing_parity_indices.into_iter().zip(recovered_parity) {
            shards[idx] = Some(recovered);
        }
        Ok(())
    }

    #[cfg(feature = "std")]
    pub(crate) fn reconstruct_internal_option_vec_par_with_policy(
        &self,
        shards: &mut [Option<Vec<F::Elem>>],
        data_only: bool,
        policy: ParallelPolicy,
    ) -> Result<(), Error>
    where
        F::Elem: Send + Sync,
    {
        self.reconstruct_internal_option_vec_par_with_stage_policies(
            shards, data_only, policy, policy,
        )
    }

    /// Reconstruct all missing shards (data and parity).
    ///
    /// Missing shards should be zero-length; present shards must have valid data.
    pub fn reconstruct<T: ReconstructShard<F>>(&self, slices: &mut [T]) -> Result<(), Error> {
        if self.is_leopard_gf8_family() {
            return self.reconstruct_leopard_gf8(slices, false);
        }
        if self.is_leopard_gf16_family() {
            return self.reconstruct_leopard_gf16(slices, false);
        }
        self.ensure_classic_family_execution()?;
        self.reconstruct_internal(slices, false)
    }

    /// Reconstruct only missing data shards (parity shards are not recovered).
    pub fn reconstruct_data<T: ReconstructShard<F>>(&self, slices: &mut [T]) -> Result<(), Error> {
        if self.is_leopard_gf8_family() {
            return self.reconstruct_leopard_gf8(slices, true);
        }
        if self.is_leopard_gf16_family() {
            return self.reconstruct_leopard_gf16(slices, true);
        }
        self.ensure_classic_family_execution()?;
        self.reconstruct_internal(slices, true)
    }

    /// Reconstruct only shards marked `true` in the `required` mask.
    pub fn reconstruct_some<T: ReconstructShard<F>>(
        &self,
        shards: &mut [T],
        required: &[bool],
    ) -> Result<(), Error> {
        if self.is_leopard_gf8_family() {
            // For leopard, reconstruct_some delegates to reconstruct (leopard always
            // recovers all shards, then the caller picks which ones they needed).
            self.reconstruct_leopard_gf8(shards, false)?;
            return Ok(());
        }
        if self.is_leopard_gf16_family() {
            self.reconstruct_leopard_gf16(shards, false)?;
            return Ok(());
        }
        self.ensure_classic_family_execution()?;
        if required.len() != self.total_shard_count {
            return Err(Error::InvalidShardFlags);
        }

        check_piece_count!(all => self, shards);

        let mut number_present = 0;
        let mut shard_len = None;

        for shard in shards.iter_mut() {
            if let Some(len) = shard.len() {
                if len == 0 {
                    return Err(Error::EmptyShard);
                }
                number_present += 1;
                if let Some(old_len) = shard_len
                    && len != old_len
                {
                    return Err(Error::IncorrectShardSize);
                }
                shard_len = Some(len);
            }
        }

        if number_present == self.total_shard_count {
            return Ok(());
        }

        if number_present < self.data_shard_count {
            return Err(Error::TooFewShardsPresent);
        }

        let shard_len = shard_len.expect("at least one shard present; qed");
        let required_data_only = required
            .iter()
            .enumerate()
            .all(|(i, required)| !*required || i < self.data_shard_count);

        let originally_missing: Vec<bool> = shards
            .iter_mut()
            .map(|shard| shard.get().is_none())
            .collect();

        if required_data_only {
            let mut valid_indices: SmallVec<[usize; 32]> =
                SmallVec::with_capacity(self.data_shard_count);
            let mut invalid_indices: SmallVec<[usize; 32]> =
                SmallVec::with_capacity(self.total_shard_count);
            let mut required_missing_data_indices: SmallVec<[usize; 32]> = SmallVec::new();

            for (index, is_missing) in originally_missing.iter().copied().enumerate() {
                if is_missing {
                    invalid_indices.push(index);
                    if index < self.data_shard_count && required[index] {
                        required_missing_data_indices.push(index);
                    }
                } else if valid_indices.len() < self.data_shard_count {
                    valid_indices.push(index);
                }
            }

            if required_missing_data_indices.is_empty() {
                return Ok(());
            }

            let data_decode_matrix = self
                .get_data_decode_matrix(&valid_indices, &invalid_indices)?;
            let mut matrix_rows: SmallVec<[&[F::Elem]; 32]> =
                SmallVec::with_capacity(required_missing_data_indices.len());
            for &idx in &required_missing_data_indices {
                matrix_rows.push(data_decode_matrix.get_row(idx));
            }

            // Borrow present input shards directly and defer output mutation until
            // after compute completes, so the required-data-only path avoids
            // cloning every valid shard into temporary owned buffers.
            let mut sub_shard_ptrs: SmallVec<[(*const F::Elem, usize); 32]> =
                SmallVec::with_capacity(valid_indices.len());
            for &idx in &valid_indices {
                let shard = shards[idx]
                    .get()
                    .expect("valid shard index must be present");
                sub_shard_ptrs.push((shard.as_ptr(), shard.len()));
            }

            let sub_shards: SmallVec<[&[F::Elem]; 32]> = sub_shard_ptrs
                .iter()
                .map(|&(ptr, len)| {
                    // SAFETY: each pointer/length pair was captured from a present
                    // shard in `shards` after full length validation. The compute
                    // phase only reads from these inputs and writes into separate
                    // owned recovery buffers, so the pointed-to shard storage
                    // remains valid and unmodified for the duration of these
                    // borrowed slices.
                    unsafe { core::slice::from_raw_parts(ptr, len) }
                })
                .collect();
            let mut output_ptrs: SmallVec<[(*mut F::Elem, usize); 32]> =
                SmallVec::with_capacity(required_missing_data_indices.len());
            for &idx in &required_missing_data_indices {
                match shards[idx].get_or_initialize(shard_len) {
                    Ok(dst) | Err(Ok(dst)) => output_ptrs.push((dst.as_mut_ptr(), dst.len())),
                    Err(Err(err)) => return Err(err),
                }
            }
            let mut outputs: SmallVec<[&mut [F::Elem]; 32]> = output_ptrs
                .iter()
                .map(|&(ptr, len)| {
                    // SAFETY: each pointer/length pair came from a distinct
                    // `get_or_initialize(shard_len)` call above after all
                    // validation completed. Required missing indices are unique,
                    // and this compute phase mutates only those output buffers.
                    unsafe { core::slice::from_raw_parts_mut(ptr, len) }
                })
                .collect();
            self.code_some_slices(&matrix_rows, &sub_shards, &mut outputs);
            drop(outputs);
        } else {
            let mut working: Vec<Option<Vec<F::Elem>>> = shards
                .iter_mut()
                .map(|shard| shard.get().map(|data| data.to_vec()))
                .collect();
            self.reconstruct(&mut working)?;

            for (i, shard) in shards.iter_mut().enumerate() {
                if !required[i] || !originally_missing[i] {
                    continue;
                }

                let recovered = working[i]
                    .as_ref()
                    .expect("recovered shard must be present");
                match shard.get_or_initialize(shard_len) {
                    Ok(dst) | Err(Ok(dst)) => dst.copy_from_slice(recovered),
                    Err(Err(err)) => return Err(err),
                }
            }
        }

        Ok(())
    }

    /// Shared implementation for Leopard GF8/GF16 reconstruction.
    ///
    /// Builds the `present`, `outputs`, and `input_data` arrays required by
    /// the Forney-based FFT decoder, calls the provided `reconstruct_fn`,
    /// then writes recovered data back into the original shard objects.
    ///
    /// Only copies present shard data into output buffers for missing shards
    /// (the Leopard decoder skips present positions). This avoids the overhead
    /// of copying all present shard data into temporary buffers.
    #[allow(clippy::needless_range_loop, clippy::type_complexity)]
    fn reconstruct_leopard_impl<T: ReconstructShard<F>>(
        &self,
        slices: &mut [T],
        reconstruct_fn: fn(
            &[bool],
            &mut [&mut [u8]],
            &[Option<&[u8]>],
            usize,
            usize,
        ) -> Result<(), Error>,
    ) -> Result<(), Error> {
        check_piece_count!(all => self, slices);

        let total = self.total_shard_count;
        let shard_len_opt: Option<usize> = slices.iter().find_map(|s| s.len());
        let Some(shard_len) = shard_len_opt else {
            return Err(Error::EmptyShard);
        };

        // SAFETY: F::Elem = u8 for all leopard codec families.
        let mut present = vec![false; total];
        let mut raw_data: Vec<Option<*const u8>> = vec![None; total];
        for i in 0..total {
            if let Some(data) = slices[i].get() {
                present[i] = true;
                raw_data[i] = Some(data as *const [F::Elem] as *const u8);
            }
        }

        // Allocate output buffers. Present shards get zeroed buffers (the decoder
        // skips them via `if present[i] { continue; }`). Missing shards also get
        // zeroed buffers which the decoder writes recovered data into.
        let mut output_bufs: Vec<Vec<u8>> = (0..total).map(|_| vec![0u8; shard_len]).collect();

        let mut outputs: Vec<&mut [u8]> = output_bufs
            .iter_mut()
            .map(|buf| buf.as_mut_slice())
            .collect();

        let mut input_data: Vec<Option<&[u8]>> = Vec::with_capacity(total);
        for i in 0..total {
            if let Some(ptr) = raw_data[i] {
                let src: &[u8] = unsafe { core::slice::from_raw_parts(ptr, shard_len) };
                input_data.push(Some(src));
            } else {
                input_data.push(None);
            }
        }

        reconstruct_fn(
            &present,
            &mut outputs,
            &input_data,
            self.data_shard_count,
            self.parity_shard_count,
        )?;

        for i in 0..total {
            if present[i] {
                continue;
            }
            let elem_slice: &[F::Elem] =
                unsafe { &*(output_bufs[i].as_slice() as *const [u8] as *const [F::Elem]) };
            match slices[i].get_or_initialize(shard_len) {
                Ok(dst) | Err(Ok(dst)) => dst.copy_from_slice(elem_slice),
                Err(Err(err)) => return Err(err),
            }
        }

        Ok(())
    }

    fn reconstruct_leopard_gf8<T: ReconstructShard<F>>(
        &self,
        slices: &mut [T],
        _data_only: bool,
    ) -> Result<(), Error> {
        self.reconstruct_leopard_impl(slices, super::leopard_gf8::reconstruct_with_tables)
    }

    fn reconstruct_leopard_gf16<T: ReconstructShard<F>>(
        &self,
        slices: &mut [T],
        _data_only: bool,
    ) -> Result<(), Error> {
        self.reconstruct_leopard_impl(slices, super::leopard::leopard_gf16_reconstruct)
    }

    pub(crate) fn get_data_decode_matrix(
        &self,
        valid_indices: &[usize],
        invalid_indices: &[usize],
    ) -> Result<Arc<crate::matrix::Matrix<F>>, Error> {
        if valid_indices.len() != self.data_shard_count {
            return Err(Error::TooFewShardsPresent);
        }

        if self.options.inversion_cache {
            #[cfg(feature = "std")]
            self.reconstruction_cache_metrics
                .requests
                .fetch_add(1, Ordering::Relaxed);

            let mut cache = self.data_decode_matrix_cache.lock();
        if let Some(entry) = cache.get(invalid_indices) {
            #[cfg(feature = "std")]
            self.reconstruction_cache_metrics
                .hits
                .fetch_add(1, Ordering::Relaxed);
                return Ok(entry.clone());
        }

            #[cfg(feature = "std")]
            self.reconstruction_cache_metrics
                .misses
                .fetch_add(1, Ordering::Relaxed);
        }
        let mut sub_matrix =
            crate::matrix::Matrix::new(self.data_shard_count, self.data_shard_count);
        for (sub_matrix_row, &valid_index) in valid_indices.iter().enumerate() {
            for c in 0..self.data_shard_count {
                sub_matrix.set(sub_matrix_row, c, self.matrix.get(valid_index, c));
            }
        }
        let data_decode_matrix = Arc::new(sub_matrix.invert().map_err(|_| {
            Error::InvalidCustomMatrix
        })?);
        if self.options.inversion_cache {
            let data_decode_matrix = data_decode_matrix.clone();
            let mut cache = self.data_decode_matrix_cache.lock();
            #[cfg(feature = "std")]
            let before_len = cache.len();
            #[cfg(feature = "std")]
            let capacity = cache.capacity();
            cache.insert(Vec::from(invalid_indices), data_decode_matrix);
            #[cfg(feature = "std")]
            if capacity > 0 && before_len >= capacity {
                self.reconstruction_cache_metrics
                    .evictions
                    .fetch_add(1, Ordering::Relaxed);
            }
            #[cfg(feature = "std")]
            self.reconstruction_cache_metrics
                .inserts
                .fetch_add(1, Ordering::Relaxed);
        }
        Ok(data_decode_matrix)
    }

    fn reconstruct_internal<T: ReconstructShard<F>>(
        &self,
        shards: &mut [T],
        data_only: bool,
    ) -> Result<(), Error> {
        check_piece_count!(all => self, shards);

        let data_shard_count = self.data_shard_count;

        let mut number_present = 0;
        let mut shard_len = None;

        for shard in shards.iter_mut() {
            if let Some(len) = shard.len() {
                if len == 0 {
                    return Err(Error::EmptyShard);
                }
                number_present += 1;
                if let Some(old_len) = shard_len
                    && len != old_len
                {
                    return Err(Error::IncorrectShardSize);
                }
                shard_len = Some(len);
            }
        }

        if number_present == self.total_shard_count {
            #[cfg(feature = "std")]
            self.runtime_profile_metrics
                .record_reconstruct(data_only, 0, 0, true);
            return Ok(());
        }

        if number_present < data_shard_count {
            return Err(Error::TooFewShardsPresent);
        }

        let shard_len = shard_len.expect("at least one shard present; qed");

        let mut sub_shards: SmallVec<[&[F::Elem]; 32]> = SmallVec::with_capacity(data_shard_count);
        let mut missing_data_slices: SmallVec<[&mut [F::Elem]; 32]> =
            SmallVec::with_capacity(self.parity_shard_count);
        let mut missing_parity_slices: SmallVec<[&mut [F::Elem]; 32]> =
            SmallVec::with_capacity(self.parity_shard_count);
        let mut valid_indices: SmallVec<[usize; 32]> = SmallVec::with_capacity(data_shard_count);
        let mut invalid_indices: SmallVec<[usize; 32]> = SmallVec::with_capacity(data_shard_count);

        for (matrix_row, shard) in shards.iter_mut().enumerate() {
            let shard_data = if matrix_row >= data_shard_count && data_only {
                shard.get().ok_or(None)
            } else {
                shard.get_or_initialize(shard_len).map_err(Some)
            };

            match shard_data {
                Ok(shard) => {
                    if sub_shards.len() < data_shard_count {
                        sub_shards.push(shard);
                        valid_indices.push(matrix_row);
                    }
                }
                Err(None) => {
                    invalid_indices.push(matrix_row);
                }
                Err(Some(x)) => {
                    let shard = x?;
                    if matrix_row < data_shard_count {
                        missing_data_slices.push(shard);
                    } else {
                        missing_parity_slices.push(shard);
                    }

                    invalid_indices.push(matrix_row);
                }
            }
        }

        #[cfg(feature = "std")]
        {
            let missing_data_count = invalid_indices
                .iter()
                .filter(|&&i| i < data_shard_count)
                .count();
            let missing_parity_count = if data_only {
                0
            } else {
                invalid_indices
                    .iter()
                    .filter(|&&i| i >= data_shard_count)
                    .count()
            };
            self.runtime_profile_metrics.record_reconstruct(
                data_only,
                missing_data_count,
                missing_parity_count,
                false,
            );
        }

        let data_decode_matrix = self
            .get_data_decode_matrix(&valid_indices, &invalid_indices)?;

        let mut matrix_rows: SmallVec<[&[F::Elem]; 32]> =
            SmallVec::with_capacity(self.parity_shard_count);

        for i_slice in invalid_indices
            .iter()
            .cloned()
            .take_while(|i| i < &data_shard_count)
        {
            matrix_rows.push(data_decode_matrix.get_row(i_slice));
        }

        #[cfg(feature = "std")]
        self.runtime_profile_metrics
            .record_reconstruct_data_stage(shard_len, matrix_rows.len());
        self.code_some_slices(&matrix_rows, &sub_shards, &mut missing_data_slices);

        if data_only {
            Ok(())
        } else {
            let mut matrix_rows: SmallVec<[&[F::Elem]; 32]> =
                SmallVec::with_capacity(self.parity_shard_count);
            let parity_rows = self.get_parity_rows();

            for i_slice in invalid_indices
                .iter()
                .cloned()
                .skip_while(|i| i < &data_shard_count)
            {
                matrix_rows.push(parity_rows[i_slice - data_shard_count]);
            }
            #[cfg(feature = "std")]
            self.runtime_profile_metrics
                .record_reconstruct_parity_stage(shard_len, matrix_rows.len());
            {
                let mut i_old_data_slice = 0;
                let mut i_new_data_slice = 0;

                let mut all_data_slices: SmallVec<[&[F::Elem]; 32]> =
                    SmallVec::with_capacity(data_shard_count);

                let mut next_maybe_good = 0;
                let mut push_good_up_to = move |data_slices: &mut SmallVec<_>, up_to| {
                    for _ in next_maybe_good..up_to {
                        data_slices.push(sub_shards[i_old_data_slice]);
                        i_old_data_slice += 1;
                    }

                    next_maybe_good = up_to + 1;
                };

                for i_slice in invalid_indices
                    .iter()
                    .cloned()
                    .take_while(|i| i < &data_shard_count)
                {
                    push_good_up_to(&mut all_data_slices, i_slice);
                    all_data_slices.push(missing_data_slices[i_new_data_slice]);
                    i_new_data_slice += 1;
                }
                push_good_up_to(&mut all_data_slices, data_shard_count);

                self.code_some_slices(&matrix_rows, &all_data_slices, &mut missing_parity_slices);
            }

            Ok(())
        }
    }
}
