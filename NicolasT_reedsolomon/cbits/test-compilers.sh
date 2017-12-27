#!/bin/bash -ue

function build() {
        local cc=$1
        shift
        local host=$1
        shift
        local cflags=$1
        local args=""

        if ! test "x$host" = "x"; then
                args="$args --host=$host"
        fi
        if ! test "x$cc" = "x"; then
                args="$args CC=$cc"
        fi
        if ! test "x$cflags" = "x"; then
                args="$args CFLAGS=$cflags"
        fi

        local oldpath=$PATH
        export PATH=~/x-tools/$host/bin:$PATH
        ./configure $args
        make
        make distclean
        export PATH=$oldpath
}

build "" x86_64-redhat-linux ""
build "clang" "" ""
build "" aarch64-unknown-linux-musleabi ""
build "" armv7-rpi2-linux-gnueabihf ""
build "" armv7-rpi2-linux-gnueabihf "-mfpu=neon"
build "" powerpc64-unknown-linux-gnu ""
build "" powerpc64-unknown-linux-gnu "-maltivec"
build "" powerpc64-unknown-linux-gnu "-mcpu=power8"
build "" powerpc-unknown-linux-gnu ""
build "" i686-w64-mingw32 ""
build "" x86_64-w64-mingw32 ""
