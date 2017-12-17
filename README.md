# reed-solomon-erasure
Rust implementation of Reed-Solomon erasure coding

## Usage
Add the following to your `Cargo.toml` :
```toml
[dependencies]
reed-solomon-erasure = "0.9.1"
```
and the following to your crate root :
```rust
extern crate reed_solomon_erasure;
```

## Test
```sh
cargo test
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
    
    assert!(r.is_parity_correct(result, None, None));
    assert_eq!(master_copy, result);
}
```

## Notes
Reed-Solomon implementation closely follows [BackBlaze's Java implementation](https://github.com/Backblaze/JavaReedSolomon), and the test suite mirrors [Klaus Post's Go implementation](https://github.com/klauspost/reedsolomon).

## License
#### BackBlaze Java Reed-Solomon implementation
The tables and main functions of ```reed_solomon/build.rs``` and ```reed_solomon/src/*``` are translated from [BackBlaze Java Implementation](https://github.com/Backblaze/JavaReedSolomon), and are under the same MIT License as used by the BackBlaze project

The source code copied directly from BackBlaze's project repo are under the MIT License as used by the project, the files are in ```BackBlaze_JavaReedSolomon```

#### Klaus Post Go Reed-Solomon implementation
The tables and test functions of ```reed_solomon/src/*``` are translated from [Klaus Post's Go Implementation](https://github.com/klauspost/reedsolomon), and are under the same MIT License as used by Klaus Post's project

The source code copied directly from Klaus Post's project repo are under the MIT License as used by the project, the files are in ```KlausPost_reedsolomon```

#### TL;DR
All files are released under MIT License
