#!/bin/bash

usage="usage: $0 -f|-n"

set -euo pipefail

runin() {
    echo
    local dir="$1"
    shift
    pushd "$dir"
    if [[ $DRY_RUN = 0 ]]
    then
        echo "$0: running: $dir $@"
        "$@"
    else
        echo "$0: DRY running: $dir $@"
    fi
    popd
}

DRY_RUN=
case "${1:-}"
in
    -n) DRY_RUN=1; ;;
    -f) DRY_RUN=0; ;;
esac

if [[ -z "$DRY_RUN" ]]
then
    echo "$usage"
    exit 0
fi

runin cmake-mode ./fetch.sh
runin undo-tree ./fetch.sh

runin . git submodule foreach git fetch --all --prune
