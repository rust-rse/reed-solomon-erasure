#!/bin/bash -ue

function abort() {
    echo $@ > /dev/stderr
    exit 1
}

if ! test x$# = x2; then
    abort "Usage: $0 CI STEP"
fi

CI_TYPE=$1
CI_STEP=$2

CI_ENV_SCRIPT=ci/$CI_TYPE/$SUITE/env.sh
if test -f $CI_ENV_SCRIPT; then
    source $CI_ENV_SCRIPT
fi

CI_SCRIPT=ci/$CI_TYPE/$SUITE/$CI_STEP.sh

export LOCAL_BIN=~/.local/bin
if test -d ~/.local/bin; then
    export PATH=~/.local/bin:$PATH
fi

if test -x $CI_SCRIPT; then
    echo "Running $CI_SCRIPT"
    exec $CI_SCRIPT
fi
