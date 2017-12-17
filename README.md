# reed-solomon-erasure
Rust implementation of Reed-Solomon erasure coding

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
