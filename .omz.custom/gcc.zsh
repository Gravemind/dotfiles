#!/bin/zsh

function whichCC() {
	echo gcc g++ $(g++ --version | sed -nr 's/[^0-9.]*([0-9]+(\.[0-9]+)+).*/\1/p') $(which gcc g++)
	echo clang clang++ $(clang++ --version | sed -nr 's/[^0-9.]*([0-9]+(\.[0-9]+)+).*/\1/p') $(which clang clang++)
	echo CC CXX $CC $CXX
}

function gcc48() {
	export CC=gcc-4.8
	export CXX=g++-4.8
	export CPP=$CXX
	whichCC
}

function gcc49() {
	export CC=gcc-4.9
	export CXX=g++-4.9
	export CPP=$CXX
	whichCC
}

function gcclast() {
	# export CC=gcc
	# export CXX=g++
	# export CPP=$CXX
	unset CC
	unset CXX
	unset CPP
	whichCC
}

function clanglast() {
	export CC=clang
	export CXX=clang++
	export CPP=$CXX
	whichCC
}

export RTAGS_BIN_PATH=$HOME/bin/rtags_bin
export RTAGS_OLD_PATH=$PATH

function rtagsoff() {
	export PATH=$RTAGS_OLD_PATH
	rehash
	whichCC
}

function rtagson() {
	export RTAGS_OLD_PATH=$PATH
	export PATH=$RTAGS_BIN_PATH:$PATH
	rehash
	whichCC
}

export CC=clang
export CXX=clang++
export CPP=$CXX

#rtagson
