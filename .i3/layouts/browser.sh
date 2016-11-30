#!/bin/bash

here="$(dirname "$(readlink -f "$0")")"

browser="${1:-firefox}"
ws="${2:-10}"

$here/append_layout_window.sh "$here/$browser.json" $ws

$browser &
disown
