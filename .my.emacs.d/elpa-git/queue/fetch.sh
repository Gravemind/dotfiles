#!/bin/bash

set -euo pipefail
set -x

cd "$(dirname "$0")"

# https://elpa.gnu.org/packages/queue.html
url="https://elpa.gnu.org/packages/queue-0.2.el"
curl -o queue.el "$url"
