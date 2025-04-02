#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

url="https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/external-completion.el"
rm -f external-completion.el
curl -o external-completion.el "$url"
