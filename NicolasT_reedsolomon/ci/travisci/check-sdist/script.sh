#!/bin/bash -xue

PKG_VERSION=`grep ^Version\: reedsolomon.cabal | awk '{ print $2 }'`

pushd cbits
./configure
make distcheck
mv reedsolomon-$PKG_VERSION.tar.gz /tmp/reedsolomon-$PKG_VERSION-cbits.tar.gz
make distclean
popd

cp ci/travisci/check-sdist/test.hs /tmp

git clean -dxf

cabal sdist

PKG_TAR=reedsolomon-$PKG_VERSION.tar.gz
test -f dist/$PKG_TAR
mv dist/$PKG_TAR /tmp/$PKG_TAR

cd /tmp

mkdir cbits
pushd cbits
tar xvf /tmp/reedsolomon-$PKG_VERSION-cbits.tar.gz
popd

mkdir haskell
pushd haskell
tar xvf /tmp/$PKG_TAR
popd

diff -Nur cbits/reedsolomon-$PKG_VERSION haskell/reedsolomon-$PKG_VERSION/cbits | \
    tee diffs
test x`wc -l < diffs` = x0

cabal install --enable-documentation -f-LLVM $PKG_TAR

cat test.hs

ghc --make test.hs

./test
