## 2.0.0
  - Complete rewrite of most code following Klaus Post's design
  - Added optimsations(parallelism, loop unrolling)
  - 4-5x faster than `1.X.X`

## 1.1.1
  - Documentation polish
  - Added documentation badge to README
  - Optimised internal matrix related operations
    - This largely means `decode_missing` is faster

## 1.1.0
  - Added more helper functions
  - Added more tests
 
## 1.0.1
  - Added more tests
  - Fixed decode_missing
    - Previously may reconstruct the missing shards with incorrect length

## 1.0.0
  - Added more tests
  - Added integration with Codecov (via kcov)
  - Code refactoring
  - Added integration with Coveralls (via kcov)

## 0.9.1
  - Code restructuring
  - Added documentation

## 0.9.0
  - Base version
