#![cfg(test)]

use errors::Error;
use errors::SBSError;

#[test]
fn test_error_to_string_is_okay() {
    assert_eq!(Error::TooFewShards.to_string(),        "Too few shards");
    assert_eq!(Error::TooManyShards.to_string(),       "Too many shards");
    assert_eq!(Error::TooFewDataShards.to_string(),    "Too few data shards");
    assert_eq!(Error::TooManyDataShards.to_string(),   "Too many data shards");
    assert_eq!(Error::TooFewParityShards.to_string(),  "Too few parity shards");
    assert_eq!(Error::TooManyParityShards.to_string(), "Too many parity shards");
    assert_eq!(Error::TooFewBufferShards.to_string(),  "Too few buffer shards");
    assert_eq!(Error::TooManyBufferShards.to_string(), "Too many buffer shards");
    assert_eq!(Error::IncorrectShardSize.to_string(),  "Incorrect shard size");
    assert_eq!(Error::TooFewShardsPresent.to_string(), "Too few shards present");
    assert_eq!(Error::EmptyShard.to_string(),          "Empty shard");
    assert_eq!(Error::InvalidShardFlags.to_string(),   "Invalid shard flags");
    assert_eq!(Error::InvalidIndex.to_string(),        "Invalid index");
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
