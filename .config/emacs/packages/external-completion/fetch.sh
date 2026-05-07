#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

# Note: external-package is builtin since emacs 30

# url="https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/external-completion.el"
# url="https://gitweb.git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;f=lisp/external-completion.el"
name="external-completion"
branch="main"
url="https://raw.githubusercontent.com/emacs-straight/$name/refs/heads/$branch/$name.el"
rm -f external-completion.el
curl -o external-completion.el -Lf "$url"
