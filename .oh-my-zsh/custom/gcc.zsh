#!/bin/zsh

function printcc() {
	echo CC CXX $(which $CC $CXX)
}

function gcc48() {
	export CC=gcc-4.8
	export CXX=g++-4.8
	export CPP=$CXX
	printcc
}

function gcc49() {
	export CC=gcc-4.9
	export CXX=g++-4.9
	export CPP=$CXX
	printcc
}

function gcclast() {
	# export CC=gcc
	# export CXX=g++
	# export CPP=$CXX
	unset CC
	unset CXX
	unset CPP
	printcc
}

function clanglast() {
	export CC=clang
	export CXX=clang++
	export CPP=$CXX
	printcc
}

gcclast
