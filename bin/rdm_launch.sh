#!/bin/bash

set -eu

rtags="$HOME/bin/rtags"

cd "$rtags"

git submodule update --init

nproc="$(nproc)"

# ./configure --without-tests
cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DBUILD_TESTING=0

make -j$nproc

echo -e '\nRDM\n'

./bin/rdm -j$(( nproc / 2 )) --watch-sources-only "$@"
