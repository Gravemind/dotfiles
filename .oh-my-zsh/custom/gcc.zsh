#!/bin/zsh

function gcc48() {
	export CC=gcc-4.8
	export CXX=g++-4.8
	export CPP=$CXX
}

function gcclast() {
	export CC=gcc
	export CXX=g++
	export CPP=$CXX
}

function clanglast() {
	export CC=clang
	export CXX=clang++
	export CPP=$CXX
}

# gcc48

