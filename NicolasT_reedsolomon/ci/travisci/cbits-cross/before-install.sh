#!/bin/bash -xue

sudo apt-get -y install libc6-ppc64el-cross libc6-dev-ppc64el-cross \
                        libc6-arm64-cross libc6-dev-arm64-cross \
                        gcc-mingw-w64-i686 gcc-mingw-w64-x86-64

mkdir -p $LOCAL_BIN

curl -L https://www.stackage.org/stack/$TRAVIS_OS_NAME-x86_64 | \
    tar xz --wildcards --strip-components=1 -C $LOCAL_BIN '*/stack'

# The `qemu-user-static` package shipping with TravisCI's Ubuntu version doesn't
# provide `qemu-ppc64le-static`, which we need to run the PowerPC tests.
# Retrieve the package from the source repository and 'install' it manually
# instead...
pushd /tmp
QEMU_USER_PKG=qemu-user-static_2.5+dfsg-5ubuntu2_amd64.deb
wget http://launchpadlibrarian.net/240266007/$QEMU_USER_PKG
dpkg -x $QEMU_USER_PKG qemu-user
mv qemu-user/usr/bin/qemu-ppc64le-static $LOCAL_BIN/
popd
