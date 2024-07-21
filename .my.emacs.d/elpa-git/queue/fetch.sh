#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

url="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/queue.el?h=externals/queue"
rm -f queue.el
curl -o queue.el "$url"
