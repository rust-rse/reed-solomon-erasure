#!/bin/bash -ue

echo "Running tests with SIMD support disabled"
stack --no-terminal build \
        --flag=reedsolomon:-SIMD \
        --flag=reedsolomon:examples \
        --pedantic \
        --test \
        --test-arguments "+RTS -N2" \
        --bench \
        --no-run-benchmarks \
        --resolver=$RESOLVER \
        --haddock \
        ${STACK_BUILD_OPTIONS[*]:-}

stack --resolver=$RESOLVER clean

echo "Running tests with default settings (SIMD support enabled)"
stack --no-terminal build \
        --flag=reedsolomon:examples \
        --pedantic \
        --test \
        --test-arguments "+RTS -N2" \
        --bench \
        --resolver=$RESOLVER \
        --haddock \
        ${STACK_BUILD_OPTIONS[*]:-}

stack --resolver=$RESOLVER exec reedsolomon-simple-bench 5 2 $(( 5 * 1024 * 1024 ))
stack --resolver=$RESOLVER exec reedsolomon-simple-bench 10 2 $(( 10 * 1024 * 1024 ))
stack --resolver=$RESOLVER exec reedsolomon-simple-bench 10 4 $(( 10 * 1024 * 1024 ))
stack --resolver=$RESOLVER exec reedsolomon-simple-bench 50 20 $(( 50 * 1024 * 1024 ))
stack --resolver=$RESOLVER exec reedsolomon-simple-bench 10 4 $(( 160 * 1024 * 1024 ))
