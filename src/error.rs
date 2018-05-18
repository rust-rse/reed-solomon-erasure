use std;
use std::fmt::Formatter;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Error {
    TooFewShards,
    TooManyShards,
    TooFewDataShards,
    TooManyDataShards,
    TooFewParityShards,
    TooManyParityShards,
    TooFewBufferShards,
    TooManyBufferShards,
    IncorrectShardSize,
    TooFewShardsPresent,
    EmptyShard,
    InvalidShardFlags,
    InvalidIndex,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            Error::TooFewShards        => write!(f, "TooFewShards"),
            Error::TooManyShards       => write!(f, "TooManyShards"),
            Error::TooFewDataShards    => write!(f, "TooFewDataShards"),
            Error::TooManyDataShards   => write!(f, "TooManyDataShards"),
            Error::TooFewParityShards  => write!(f, "TooFewParityShards"),
            Error::TooManyParityShards => write!(f, "TooManyParityShards"),
            Error::TooFewBufferShards  => write!(f, "TooFewBufferShards"),
            Error::TooManyBufferShards => write!(f, "TooManyBufferShards"),
            Error::IncorrectShardSize  => write!(f, "IncorrectShardSize"),
            Error::TooFewShardsPresent => write!(f, "TooFewShardsPresent"),
            Error::EmptyShard          => write!(f, "EmptyShard"),
            Error::InvalidShardFlags   => write!(f, "InvalidShardFlags"),
            Error::InvalidIndex        => write!(f, "InvalidIndex"),
        }
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::TooFewShards        => "reed_solomon_erasure : Error : Too few shards",
            Error::TooManyShards       => "reed_solomon_erasure : Error : Too many shards",
            Error::TooFewDataShards    => "reed_solomon_erasure : Error : Too few data shards",
            Error::TooManyDataShards   => "reed_solomon_erasure : Error : Too many data shards",
            Error::TooFewParityShards  => "reed_solomon_erasure : Error : Too few parity shards",
            Error::TooManyParityShards => "reed_solomon_erasure : Error : Too many parity shards",
            Error::TooFewBufferShards  => "reed_solomon_erasure : Error : Too few buffer shards",
            Error::TooManyBufferShards => "reed_solomon_erasure : Error : Too many buffer shards",
            Error::IncorrectShardSize  => "reed_solomon_erasure : Error : Incorrect shard size",
            Error::TooFewShardsPresent => "reed_solomon_erasure : Error : Too few shards present",
            Error::EmptyShard          => "reed_solomon_erasure : Error : Empty shard",
            Error::InvalidShardFlags   => "reed_solomon_erasure : Error : Invalid shard flags",
            Error::InvalidIndex        => "reed_solomon_erasure : Error : Invalid index",
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum SBSError {
    TooManyCalls,
    LeftoverShards,
    RSError(Error)
}

impl std::fmt::Display for SBSError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            SBSError::TooManyCalls   => write!(f, "TooManyCalls"),
            SBSError::LeftoverShards => write!(f, "LeftoverShards"),
            SBSError::RSError(e)     => write!(f, "RSError : {}", e),
        }
    }
}

impl std::error::Error for SBSError {
    fn description(&self) -> &str {
        match *self {
            SBSError::TooManyCalls => "reed_solomon_erasure : ShardByShard : too many calls",
            SBSError::LeftoverShards => "reed_solomon_erasure : ShardByShard too many calls",
            SBSError::RSError(e)     =>
                match e {
                    Error::TooFewShards        => "reed_solomon_erasure : ShardByShard::RSError : Too few shards",
                    Error::TooManyShards       => "reed_solomon_erasure : ShardByShard::RSError : Too many shards",
                    Error::TooFewDataShards    => "reed_solomon_erasure : ShardByShard::RSError : Too few data shards",
                    Error::TooManyDataShards   => "reed_solomon_erasure : ShardByShard::RSError : Too many data shards",
                    Error::TooFewParityShards  => "reed_solomon_erasure : ShardByShard::RSError : Too few parity shards",
                    Error::TooManyParityShards => "reed_solomon_erasure : ShardByShard::RSError : Too many parity shards",
                    Error::TooFewBufferShards  => "reed_solomon_erasure : ShardByShard::RSError : Too few buffer shards",
                    Error::TooManyBufferShards => "reed_solomon_erasure : ShardByShard::RSError : Too many buffer shards",
                    Error::IncorrectShardSize  => "reed_solomon_erasure : ShardByShard::RSError : Incorrect shard size",
                    Error::TooFewShardsPresent => "reed_solomon_erasure : ShardByShard::RSError : Too few shards present",
                    Error::EmptyShard          => "reed_solomon_erasure : ShardByShard::RSError : Empty shard",
                    Error::InvalidShardFlags   => "reed_solomon_erasure : ShardByShard::RSError : Invalid shard flags",
                    Error::InvalidIndex        => "reed_solomon_erasure : ShardByShard::RSError : Invalid index",
                }
        }
    }
}
