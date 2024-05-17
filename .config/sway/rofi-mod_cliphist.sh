#!/usr/bin/env bash

if [[ -z "$1" ]]; then
    cliphist list
else
    {
        # cliphist decode <<<"$1" | wl-copy
        cliphist decode <<<"$1" | pee 'wl-copy' 'wl-copy --primary'
    } >&2
fi
