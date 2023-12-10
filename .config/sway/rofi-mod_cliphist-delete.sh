#!/usr/bin/env bash

list() {
    echo "**ALL**"
    cliphist list
}

if [[ -z "$1" ]]; then
    list
elif [[ "$1" = "**ALL**" ]]; then
    cliphist wipe
else
    cliphist delete <<<"$1"
    list
fi
