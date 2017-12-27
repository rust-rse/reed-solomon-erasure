#!/bin/bash -ue

set -x

case "$TRAVIS_OS_NAME" in
    linux)
            TAR_WILDCARDS=--wildcards;
            export TAR_WILDCARDS;
            case "${LLVM_VERSION:-x}" in
                x)
                    STACK_BUILD_OPTIONS="--flag=reedsolomon:-LLVM";;
                *)
                    STACK_BUILD_OPTIONS=("--ghc-options" "-pgmlo opt-$LLVM_VERSION -pgmlc llc-$LLVM_VERSION");;
            esac
            export STACK_BUILD_OPTIONS;;
    osx)
            STACK_BUILD_OPTIONS="--flag=reedsolomon:-LLVM";
            export STACK_BUILD_OPTIONS;;
esac

# TravisCI built-in functions
# These aren't inherited by the shell... Copied from
# https://github.com/travis-ci/travis-build/blob/136da0ef6a17b9be215f4885926823e3625e0be0/lib/travis/build/templates/header.sh
ANSI_RED="\033[31;1m"
ANSI_RESET="\033[0m"

travis_retry() {
  local result=0
  local count=1
  while [ $count -le 3 ]; do
    [ $result -ne 0 ] && {
      echo -e "\n${ANSI_RED}The command \"$@\" failed. Retrying, $count of 3.${ANSI_RESET}\n" >&2
    }
    "$@"
    result=$?
    [ $result -eq 0 ] && break
    count=$(($count + 1))
    sleep 1
  done

  [ $count -gt 3 ] && {
    echo -e "\n${ANSI_RED}The command \"$@\" failed 3 times.${ANSI_RESET}\n" >&2
  }

  return $result
}

export ANSI_RED
export ANSI_RESET
export -f travis_retry
