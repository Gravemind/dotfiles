#!/bin/bash

set -euo pipefail
set -x

url="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/undo-tree/undo-tree.el"
curl -o undo-tree.el "$url"

