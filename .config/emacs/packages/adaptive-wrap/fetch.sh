#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

url="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/adaptive-wrap.el?h=externals/adaptive-wrap"
rm -f adaptive-wrap.el
curl -o adaptive-wrap.el "$url"
