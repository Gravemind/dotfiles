#!/bin/bash
#
# (Makes sure the two wl-paste are killed on TERM)
#

set -euo pipefail

pids=()

bgrun() {
    "$@" &
    pids+=($!)
}

cleanup() {
    kill -TERM "${pids[@]}" 2>/dev/null
}

trap cleanup EXIT

bgrun wl-paste --watch cliphist store
bgrun wl-paste -p --watch cliphist store

wait
