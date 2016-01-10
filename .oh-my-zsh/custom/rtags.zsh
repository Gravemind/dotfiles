#!/bin/zsh

export RTAGS_BIN_PATH=$HOME/bin/rtags_bin
export RTAGS_OLD_PATH=$PATH

function printgcc() {
	echo gcc g++ $(which gcc g++)
}

function rtagsoff() {
	export PATH=$RTAGS_OLD_PATH
	rehash
	printgcc
}

function rtagson() {
	export RTAGS_OLD_PATH=$PATH
	export PATH=$RTAGS_BIN_PATH:$PATH
	rehash
	printgcc
}

# rtagson

