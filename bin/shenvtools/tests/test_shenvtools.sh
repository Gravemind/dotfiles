#!/bin/bash
# OR zsh

assert_test() {
    if ! test "$@"
    then
        echo "test failed: $*"
        exit 1
    fi
}

set -ue

source ../shenvtools.sh

VAR="."
_shenvtools_makeabs VAR
assert_test "$VAR" "=" "$PWD"

VAR=".."
_shenvtools_makeabs VAR
assert_test "$VAR" "=" "$(cd ..; pwd)"

VAR=""
envappend VAR : toto
assert_test "$VAR" "=" "toto"

VAR=""
envprepend VAR : toto
assert_test "$VAR" "=" "toto"

VAR="::something:already::"
envappend VAR : toto
assert_test "$VAR" "=" "something:already:toto"

VAR="::something:already::"
envprepend VAR : toto
assert_test "$VAR" "=" "toto:something:already"
