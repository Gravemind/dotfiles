#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

# url="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/queue.el?h=externals/queue"
# url="https://gitweb.git.savannah.gnu.org/gitweb/?p=emacs/elpa.git;a=blob_plain;f=queue.el;hb=refs/heads/externals/queue"
name="queue"
url="https://raw.githubusercontent.com/emacs-straight/$name/refs/heads/master/$name.el"
rm -f queue.el
curl -o queue.el -Lf "$url"
