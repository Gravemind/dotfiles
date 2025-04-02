#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

url="https://raw.githubusercontent.com/llvm/llvm-project/main/clang/tools/clang-format/clang-format.el"

rm -f -- clang-format.el*
curl -o clang-format.el "$url"

echo OK
