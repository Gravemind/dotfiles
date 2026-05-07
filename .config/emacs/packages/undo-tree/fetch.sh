#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

# url="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/undo-tree.el?h=externals/undo-tree"
# url="https://gitweb.git.savannah.gnu.org/gitweb/?p=emacs/elpa.git;a=blob_plain;f=undo-tree.el;hb=refs/heads/externals/undo-tree"
name="undo-tree"
url="https://raw.githubusercontent.com/emacs-straight/$name/refs/heads/master/$name.el"
rm -f undo-tree.el
curl -o undo-tree.el -Lf "$url"
