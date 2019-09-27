extern crate reed_solomon_erasure;
extern crate wasm_bindgen;

use reed_solomon_erasure::*;
use wasm_bindgen::prelude::*;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

pub const RESULT_OK: u8 = 0;
pub const RESULT_ERROR_TOO_FEW_SHARDS: u8 = 1;
pub const RESULT_ERROR_TOO_MANY_SHARDS: u8 = 2;
pub const RESULT_ERROR_TOO_FEW_DATA_SHARDS: u8 = 3;
pub const RESULT_ERROR_TOO_MANY_DATA_SHARDS: u8 = 4;
pub const RESULT_ERROR_TOO_FEW_PARITY_SHARDS: u8 = 5;
pub const RESULT_ERROR_TOO_MANY_PARITY_SHARDS: u8 = 6;
pub const RESULT_ERROR_TOO_FEW_BUFFER_SHARDS: u8 = 7;
pub const RESULT_ERROR_TOO_MANY_BUFFER_SHARDS: u8 = 8;
pub const RESULT_ERROR_INCORRECT_SHARD_SIZE: u8 = 9;
pub const RESULT_ERROR_TOO_FEW_SHARDS_PRESENT: u8 = 10;
pub const RESULT_ERROR_EMPTY_SHARD: u8 = 11;
pub const RESULT_ERROR_INVALID_SHARD_FLAGS: u8 = 12;
pub const RESULT_ERROR_INVALID_INDEX: u8 = 13;

fn result_to_number(result: Result<(), Error>) -> u8 {
    return match result {
        Ok(()) => RESULT_OK,
        Err(Error::TooFewShards) => RESULT_ERROR_TOO_FEW_SHARDS,
        Err(Error::TooManyShards) => RESULT_ERROR_TOO_MANY_SHARDS,
        Err(Error::TooFewDataShards) => RESULT_ERROR_TOO_FEW_DATA_SHARDS,
        Err(Error::TooManyDataShards) => RESULT_ERROR_TOO_MANY_DATA_SHARDS,
        Err(Error::TooFewParityShards) => RESULT_ERROR_TOO_FEW_PARITY_SHARDS,
        Err(Error::TooManyParityShards) => RESULT_ERROR_TOO_MANY_PARITY_SHARDS,
        Err(Error::TooFewBufferShards) => RESULT_ERROR_TOO_FEW_BUFFER_SHARDS,
        Err(Error::TooManyBufferShards) => RESULT_ERROR_TOO_MANY_BUFFER_SHARDS,
        Err(Error::IncorrectShardSize) => RESULT_ERROR_INCORRECT_SHARD_SIZE,
        Err(Error::TooFewShardsPresent) => RESULT_ERROR_TOO_FEW_SHARDS_PRESENT,
        Err(Error::EmptyShard) => RESULT_ERROR_EMPTY_SHARD,
        Err(Error::InvalidShardFlags) => RESULT_ERROR_INVALID_SHARD_FLAGS,
        Err(Error::InvalidIndex) => RESULT_ERROR_INVALID_INDEX,
    };
}

#[wasm_bindgen]
pub fn encode(shards: &mut [u8], data_shards: usize, parity_shards: usize) -> u8 {
    let reed_solomon = ReedSolomon::new(data_shards, parity_shards).unwrap();
    let shard_size = shards.len() / (data_shards + parity_shards);

    let mut separate_slice_shards: Vec<_> = shards
        .chunks_exact_mut(shard_size)
        .collect();

    return result_to_number(
        reed_solomon.encode((&mut separate_slice_shards).as_mut_slice())
    );
}

#[wasm_bindgen]
pub fn reconstruct(
    shards: &mut [u8],
    data_shards: usize,
    parity_shards: usize,
    shards_available: &[u8]
) -> u8 {
    let reed_solomon = ReedSolomon::new(data_shards, parity_shards).unwrap();
    let shard_size = shards.len() / (data_shards + parity_shards);

    let mut separate_slice_shards: Vec<_> = shards
        .chunks_exact_mut(shard_size)
        .collect();

    let shards_available_slice: Vec<_> = shards_available
        .iter()
        .map(|&num| {
            num == 1u8
        })
        .collect();

    return result_to_number(
        reed_solomon.reconstruct_data(
            &mut separate_slice_shards,
            &shards_available_slice
        )
    );
}
