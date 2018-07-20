#!/bin/bash

set -euo pipefail
set -x

for cmd in "$@"
do
    echo "$cmd" | socat - /tmp/mpvsocket
    sleep 0.1
done
