# reed-solomon-erasure
[![Build Status](https://travis-ci.org/darrenldl/reed-solomon-erasure.svg?branch=master)](https://travis-ci.org/darrenldl/reed-solomon-erasure)
[![codecov](https://codecov.io/gh/darrenldl/reed-solomon-erasure/branch/master/graph/badge.svg)](https://codecov.io/gh/darrenldl/reed-solomon-erasure)
[![Coverage Status](https://coveralls.io/repos/github/darrenldl/reed-solomon-erasure/badge.svg?branch=master)](https://coveralls.io/github/darrenldl/reed-solomon-erasure?branch=master)
[![Crates](https://img.shields.io/crates/v/reed-solomon-erasure.svg)](https://crates.io/crates/reed-solomon-erasure)
[![Documentation](https://docs.rs/reed-solomon-erasure/badge.svg)](https://docs.rs/reed-solomon-erasure)

Rust implementation of Reed-Solomon erasure coding

This is a port of [BackBlaze's Java implementation](https://github.com/Backblaze/JavaReedSolomon) and [Klaus Post's Go implementation](https://github.com/klauspost/reedsolomon).

Version `1.X.X` copies BackBlaze's implementation, and is less performant as there were fewer places where parallelism could be added.

Version `2.X.X` copies Klaus Post's implementation.

See [Notes](#notes) and [License](#license) section for details.

## Usage
Add the following to your `Cargo.toml`
```toml
[dependencies]
reed-solomon-erasure = "2.0"
```
and the following to your crate root
```rust
extern crate reed_solomon_erasure;
```

## Example
```rust
#[macro_use(shards)]
extern crate reed_solomon_erasure;

use reed_solomon_erasure::*;

fn main () {
    let r = ReedSolomon::new(3, 2); // 3 data shards, 2 parity shards

    let mut master_copy = shards!([0, 1,  2,  3],
                                  [4, 5,  6,  7],
                                  [8, 9, 10, 11],
                                  [0, 0,  0,  0], // last 2 rows are parity shards
                                  [0, 0,  0,  0]);

    // Construct the parity shards
    r.encode_parity(&mut master_copy, None, None);
    
    // Make a copy and transform it into option shards arrangement
    // for feeding into decode_missing
    let mut shards = shards_into_option_shards(master_copy.clone());

    // We can remove up to 2 shards, which may be data or parity shards
    shards[0] = None;
    shards[4] = None;
    
    // Try to reconstruct missing shards
    r.decode_missing(&mut shards, None, None).unwrap();
    
    // Convert back to normal shard arrangement
    let result = option_shards_into_shards(shards);
    
    assert!(r.is_parity_correct(&result, None, None));
    assert_eq!(master_copy, result);
}
```

## Performance
Machine : laptop with `Intel(R) Core(TM) i5-3337U CPU @ 1.80GHz (max 2.70GHz) 2 Cores 4 Threads`

Version `1.X.X`, `2.X.X` do not utilise SIMD, as stable Rust still does not support SIMD yet. And also I am clueless on writing assembly code in general or linking to assembly code in Rust code(any help on the assembly code side of things would be really nice - shoot me an email/open issue/open PR/etc if you'd like to help). So for the time being, the library is written in pure Rust.

Version `2.X.X` is roughly 4-5x faster than version `1.X.X` for encoding, depending on threading etc, but is always faster than version `1.X.X`.

Klaus Post's Go implementation is roughly 7-8x faster for encoding compared to version `2.X.X`, depending on threading etc, but is always faster as it supports SIMD operations.

|Configuration| Klaus Post's | 2.X.X | 1.X.X |
|---|---|---|---|
|10x2x1M|~7800MB/s|~1100MB/s|~250MB/s|

## Changelog
[Changelog](CHANGELOG.md)

## Notes
The `1.X.X` implementation mostly copies [BackBlaze's Java implementation](https://github.com/Backblaze/JavaReedSolomon).

The `2.X.X` implementation mostly copies [Klaus Post's Go implementation](https://github.com/klauspost/reedsolomon).

The test suite for both versions copies [Klaus Post's Go implementation](https://github.com/klauspost/reedsolomon).

## License
#### BackBlaze Java Reed-Solomon implementation
The tables and main functions of ```build.rs``` are translated from [BackBlaze Java Implementation](https://github.com/Backblaze/JavaReedSolomon), and are under the same MIT License as used by the BackBlaze project

The source code copied directly from BackBlaze's project repo are under the MIT License as used by the project, the files are in ```BackBlaze_JavaReedSolomon```

#### Klaus Post Go Reed-Solomon implementation
The tables and main functions of ```src/*``` are translated from [Klaus Post's Go Implementation](https://github.com/klauspost/reedsolomon), and are under the same MIT License as used by Klaus Post's project

The source code copied directly from Klaus Post's project repo are under the MIT License as used by the project, the files are in ```KlausPost_reedsolomon```

#### TL;DR
All files are released under MIT License
