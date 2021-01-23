#!/bin/bash
# OR zsh

assert_test() {
    if ! test "$@" ; then
        echo "test failed: $*"
        exit 1
    fi
}
assert_in() {
    if [[ "$2" = *"$1"* ]]; then
        true
    else
        echo "test failed: $1 in $2"
        exit 1
    fi
}
assert_not_in() {
    if [[  "$2" = *"$1"* ]]; then
        echo "test failed: $1 not in $2"
        exit 1
    else
        true
    fi
}

set -ue

source ../shenvtools.sh

tmpdir="$(mktemp -d -t shenvtools_usecase_test.XXXXXXXXXX)"
trap "{ rm -rf $tmpdir; }" EXIT
cd "$tmpdir"

mkdir -p bin
touch bin/shtools_test_bin
chmod +x bin/shtools_test_bin

oldpath="$PATH"

addpath bin
assert_test "$PATH" "=" "$PWD/bin:$oldpath"
rmpath bin
assert_test "$PATH" "=" "$oldpath"


export LD_LIBRARY_PATH="" # empty it for the test

mkdir -p lib64
addlib lib64
assert_test "$LD_LIBRARY_PATH" "=" "$PWD/lib64"
rmlib lib64
assert_test -z "$LD_LIBRARY_PATH"


mkdir -p share/man/man1
touch share/man/man1/shtools_test_bin.1.gz

addtool .
assert_test "$(man -w shtools_test_bin)" "=" "$PWD/share/man/man1/shtools_test_bin.1.gz"
assert_test "$(command -v shtools_test_bin)" "=" "$PWD/bin/shtools_test_bin"
assert_in "$PWD" "$LD_LIBRARY_PATH"
rmtool .
assert_not_in "$PWD" "$PATH"
assert_not_in "$PWD" "$LD_LIBRARY_PATH"
assert_not_in "$PWD" "$MANPATH"
