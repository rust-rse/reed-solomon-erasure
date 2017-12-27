#!/bin/bash -ue

mkdir -p $LOCAL_BIN

travis_retry curl -L https://www.stackage.org/stack/$TRAVIS_OS_NAME-x86_64 | \
    tar xz ${TAR_WILDCARDS:-} --strip-components=1 -C $LOCAL_BIN '*/stack'
