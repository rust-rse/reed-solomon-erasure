# Reed-Solomon
[![Travis CI][3]][4]
[![Circle CI][5]][6]
[![Appveyor][7]][8]

[3]: https://travis-ci.org/NicolasT/reedsolomon.svg?branch=master
[4]: https://travis-ci.org/NicolasT/reedsolomon
[5]: https://circleci.com/gh/NicolasT/reedsolomon/tree/master.svg?style=svg
[6]: https://circleci.com/gh/NicolasT/reedsolomon/tree/master
[7]: https://ci.appveyor.com/api/projects/status/wrxftgosavg0vtha/branch/master?svg=true
[8]: https://ci.appveyor.com/project/NicolasT/reedsolomon/branch/master

Reed-Solomon Erasure Coding in Haskell, with speeds exceeding multiple GB/s/cpu core implemented in pure Haskell (and some SIMD C/assembler).

This is a Haskell port of the [GolangReedSolomon](https://github.com/klauspost/reedsolomon) library released by [Klaus Post](http://klauspost.com/), wich is a port of the [JavaReedSolomon](https://github.com/Backblaze/JavaReedSolomon) library released by [Backblaze](http://backblaze.com), with some additional optimizations.

For an introduction on erasure coding, see the post on the [Backblaze blog](https://www.backblaze.com/blog/reed-solomon/).

Package home: https://github.com/NicolasT/reedsolomon

# Performance
Performance depends mainly on the number of parity shards. In rough terms, doubling the number of parity shards will double the encoding time.

Here are the throughput numbers with some different selections of data and parity shards. For reference each shard is 1MB random data, and 1 CPU core is used for encoding.

<table>
  <thead>
    <tr>
      <th>Data</th>
      <th>Parity</th>
      <th>Parity</th>
      <th>SSSE3 MB/s</th>
      <th>AVX2 MB/s</th>
    </tr>
  </thead>
  <tbody>
    <tr><td>5</td><td>2</td><td>40%</td><td>3641,66</td><td>3987,24</td></tr>
    <tr><td>10</td><td>2</td><td>20%</td><td>3951,01</td><td>4444,44</td></tr>
    <tr><td>10</td><td>4</td><td>40%</td><td>1821,16</td><td>1927,90</td></tr>
    <tr><td>50</td><td>20</td><td>40%</td><td>398,09</td><td>431,78</td></tr>
  </tbody>
</table>

Example of performance on Intel(R) Core(TM) i7-4600U CPU @ 3.30GHz - 2 physical cores, 4 logical cores (note: `/proc/cpuinfo` mentions 2.10GHz only). The example uses 10 blocks with 16MB data each and 4 parity blocks.

<table>
  <thead>
    <tr>
      <th>Threads</th>
      <th>SSSE3 MB/s</th>
      <th>AVX2 MB/s</th>
      <th>Speed</th>
    </tr>
  </thead>
  <tbody>
    <tr><td>1</td><td>1551,89</td><td>1588,88</td><td>100%</td></tr>
  </tbody>
</table>

# Links
* [Backblaze Open Sources Reed-Solomon Erasure Coding Source Code](https://www.backblaze.com/blog/reed-solomon/).
* [GolangReedSolomon](https://github.com/klauspost/reedsolomon). Compatible Go library by Klaus Post.
* [JavaReedSolomon](https://github.com/Backblaze/JavaReedSolomon). Compatible java library by Backblaze.
* [go-erasure](https://github.com/somethingnew2-0/go-erasure). A similar library using cgo, slower in my tests.
* [Screaming Fast Galois Field Arithmetic](http://www.snia.org/sites/default/files2/SDC2013/presentations/NewThinking/EthanMiller_Screaming_Fast_Galois_Field%20Arithmetic_SIMD%20Instructions.pdf). Basis for SSE3 optimizations.

# License

This code, as the original [GolangReedSolomon](https://github.com/klauspost/reedsolomon) and [JavaReedSolomon](https://github.com/Backblaze/JavaReedSolomon) is published under an MIT license. See LICENSE file for more information.
