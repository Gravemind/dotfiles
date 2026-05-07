#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

# Note: project is builtin since at least emacs 27

# url="https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/project.el"
# url="https://gitweb.git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;f=lisp/progmodes/project.el"
name="project"
url="https://raw.githubusercontent.com/emacs-straight/$name/refs/heads/master/$name.el"
rm -f project.el
curl -o project.el -Lf "$url"
