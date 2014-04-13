#!/bin/zsh

export RTAGS_PATH=$HOME/bin/rtags
export RTAGS_OLD_PATH=$PATH

function rtagsoff() {
	export PATH=$RTAGS_OLD_PATH
	rehash
}

function rtagson() {
	export RTAGS_OLD_PATH=$PATH
	export PATH=$RTAGS_PATH/bin:$PATH
	rehash
}
