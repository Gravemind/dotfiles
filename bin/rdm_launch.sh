#!/bin/bash

set -eu

rtags="$HOME/bin/rtags"

cd "$rtags"

git submodule update --init

nproc="$(nproc)"

cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .

make -j$nproc

echo -e '\nRDM\n'

./bin/rdm -j$(( nproc / 2 )) --watch-sources-only "$@"
