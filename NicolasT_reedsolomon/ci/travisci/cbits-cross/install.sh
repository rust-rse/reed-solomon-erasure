#!/bin/bash -xue

pushd cbits
autoreconf -fvi
popd

stack \
    --no-terminal \
    setup \
    --resolver=$RESOLVER

stack \
    --no-terminal \
    install \
    --resolver=$RESOLVER \
    bytestring process process-extras QuickCheck
