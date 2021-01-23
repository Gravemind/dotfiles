#!/bin/bash

here="$(cd "$(dirname "$0")"; pwd)"
cd "$here"

log() {
    echo "
  $*
"
}

unset PS4

for sh in "bash" "bash -u" "bash -e" "bash -eu" sh zsh
do
    for test in test_shenvtools.sh test_usecase.sh
    do
        log "Test $sh $test"
        $sh "$test" || {
            log "Re-run test with -x: $sh -x $test"
            $sh -x "$test"
            log "Failed test $sh $test"
            exit 1
        }
    done
done
