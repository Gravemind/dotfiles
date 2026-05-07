#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

# Note: xref is builtin since emacs 29

# url="https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/xref.el"
# url="https://gitweb.git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;f=lisp/progmodes/xref.el"
name="xref"
url="https://raw.githubusercontent.com/emacs-straight/$name/refs/heads/master/$name.el"
rm -f xref.el
curl -o xref.el -Lf "$url"
