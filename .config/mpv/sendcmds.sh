#!/bin/bash

set -euo pipefail
set -x

case "${1:-}"
in
    command)
        shift 1
        jq '{command: $ARGS.positional}' -n -c --args -- "$@" | tee /dev/stderr | socat - /tmp/mpvsocket
        sleep 0.1
        ;;
    *)
        for cmd in "$@"
        do
            echo "$cmd" | socat - /tmp/mpvsocket
            sleep 0.1
        done
        ;;
esac
