#!/bin/bash -ue

pushd cbits
autoreconf -fvi
popd

stack \
    --no-terminal \
    setup \
    --resolver=$RESOLVER

stack \
    --no-terminal \
    build \
    ${STACK_BUILD_OPTIONS[*]:-} \
    --flag=reedsolomon:examples \
    --test \
    --bench \
    --only-snapshot \
    --resolver=$RESOLVER \
    --haddock
