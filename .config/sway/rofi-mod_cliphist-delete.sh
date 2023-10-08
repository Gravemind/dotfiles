#!/usr/bin/env bash

if [[ -z "$1" ]]; then
    echo "**ALL**"
    cliphist list
elif [[ "$1" = "**ALL**" ]]; then
    cliphist wipe
else
    cliphist delete <<<"$1"
fi
