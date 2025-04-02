#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

url="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/undo-tree.el?h=externals/undo-tree"
rm -f undo-tree.el
curl -o undo-tree.el "$url"
