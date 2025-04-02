#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

url="https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/xref.el"
rm -f xref.el
curl -o xref.el "$url"
