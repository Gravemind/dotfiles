#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

url="https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/project.el"
rm -f project.el
curl -o project.el "$url"
