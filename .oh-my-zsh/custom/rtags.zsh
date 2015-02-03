#!/bin/zsh

export RTAGS_BIN_PATH=$HOME/bin/rtags_bin
export RTAGS_OLD_PATH=$PATH

function rtagsoff() {
	export PATH=$RTAGS_OLD_PATH
    which gcc
    which g++
	rehash
}

function rtagson() {
	export RTAGS_OLD_PATH=$PATH
	export PATH=$RTAGS_BIN_PATH:$PATH
    which gcc
    which g++
	rehash
}
