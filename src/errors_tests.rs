#![cfg(test)]

use errors::Error;
use errors::SBSError;

#[test]
fn test_error_to_string_is_okay() {
    assert_eq!(Error::TooFewShards.to_string(),        "The number of provided shards is smaller than the one in codec");
    assert_eq!(Error::TooManyShards.to_string(),       "The number of provided shards is greater than the one in codec");
    assert_eq!(Error::TooFewDataShards.to_string(),    "The number of provided data shards is smaller than the one in codec");
    assert_eq!(Error::TooManyDataShards.to_string(),   "The number of provided data shards is greater than the one in codec");
    assert_eq!(Error::TooFewParityShards.to_string(),  "The number of provided parity shards is smaller than the one in codec");
    assert_eq!(Error::TooManyParityShards.to_string(), "The number of provided parity shards is greater than the one in codec");
    assert_eq!(Error::TooFewBufferShards.to_string(),  "The number of provided buffer shards is smaller than the number of parity shards in codec");
    assert_eq!(Error::TooManyBufferShards.to_string(), "The number of provided buffer shards is greater than the number of parity shards in codec");
    assert_eq!(Error::IncorrectShardSize.to_string(),  "At least one of the provided shards is not of the correct size");
    assert_eq!(Error::TooFewShardsPresent.to_string(), "The number of shards present is smaller than number of parity shards, cannot reconstruct missing shards");
    assert_eq!(Error::EmptyShard.to_string(),          "The first shard provided is of zero length");
    assert_eq!(Error::InvalidShardFlags.to_string(),   "The number of flags does not match the total number of shards");
    assert_eq!(Error::InvalidIndex.to_string(),        "The data shard index provided is greater or equal to the number of data shards in codec");
}

#[test]
fn test_sbserror_to_string_is_okay() {
    assert_eq!(SBSError::TooManyCalls.to_string(),   "Too many calls");
    assert_eq!(SBSError::LeftoverShards.to_string(), "Leftover shards");
}

#[test]
fn test_error_display_does_not_panic() {
    println!("{}", Error::TooFewShards);
}

#[test]
fn test_sbserror_display_does_not_panic() {
    println!("{}", SBSError::TooManyCalls);
}
