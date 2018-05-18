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

impl Error {
    fn to_string(&self) -> &str {
        match *self {
            Error::TooFewShards        => "Too few shards",
            Error::TooManyShards       => "Too many shards",
            Error::TooFewDataShards    => "Too few data shards",
            Error::TooManyDataShards   => "Too many data shards",
            Error::TooFewParityShards  => "Too few parity shards",
            Error::TooManyParityShards => "Too many parity shards",
            Error::TooFewBufferShards  => "Too few buffer shards",
            Error::TooManyBufferShards => "Too many buffer shards",
            Error::IncorrectShardSize  => "Incorrect shard size",
            Error::TooFewShardsPresent => "Too few shards present",
            Error::EmptyShard          => "Empty shard",
            Error::InvalidShardFlags   => "Invalid shard flags",
            Error::InvalidIndex        => "Invalid index",
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.to_string())
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        self.to_string()
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum SBSError {
    TooManyCalls,
    LeftoverShards,
    RSError(Error),
}

impl SBSError {
    fn to_string(&self) -> &str {
        match *self {
            SBSError::TooManyCalls   => "Too many calls",
            SBSError::LeftoverShards => "Leftover shards",
            SBSError::RSError(ref e) => e.to_string(),
        }
    }
}

impl std::fmt::Display for SBSError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.to_string())
    }
}

impl std::error::Error for SBSError {
    fn description(&self) -> &str {
        self.to_string()
    }
}
