#!/bin/bash

sanitize() {
    sed -E '
s/[^a-zA-Z0-9\.-]+/_/g
s/[_-]*-[_-]*/-/g
s/[_-]*\.[_-]*/\./g
s/^[_-]+//g
s/[_-]+$//g
'
}

if [[ -z "$1" ]]; then
    cliphist -preview-width 1000 list | sed -E 's/^\s*[0-9]+\s+//g' | sanitize
else
    echo "$*" | wl-copy
fi
