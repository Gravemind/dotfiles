#!/bin/zsh

ENABLE_RTAGS=0
ENABLE_CCACHE=0

_pathaddorremove() {
	local enable="$1"
	local mypath="$2"
	export PATH=$(echo "$PATH" | sed "s#${mypath}:##g")
	if [[ "$enable" = "1" ]]
	then
		export PATH="${mypath}:$PATH"
	fi
}

whichcc() {
	_pathaddorremove "$ENABLE_CCACHE" "$HOME/bin/ccache_bin"
	_pathaddorremove "$ENABLE_RTAGS" "$HOME/bin/rtags_bin"
	rehash

	local cc="${CC:-cc}"
	local allcc=$(which -a $cc)

	[[ ! "$allcc" == *rtags_bin* ]]
	local hasrtags=$?
	[[ ! "$allcc" == *ccache_bin* ]]
	local hasccache=$?

	if [[ "$hasccache" != "$ENABLE_CCACHE" ]]
	then
		echo ccache not correctly setup !
		ENABLE_CCACHE=$hasccache
	fi
	if [[ "$hasrtags" != "$ENABLE_RTAGS" ]]
	then
		echo rtags not correctly setup !
		ENABLE_RTAGS=$hasrtags
	fi

	echo -n "CC: $cc, "
	if [[ "$ENABLE_RTAGS" = "1" ]]; then echo -n "$fg_bold[green]rtags ON$reset_color"; else; echo -n "$fg_bold[black]rtags OFF$reset_color"; fi
	echo -n ', '
	if [[ "$ENABLE_CCACHE" = "1" ]]; then echo -n "$fg_bold[green]ccache ON$reset_color"; else; echo -n "$fg_bold[black]cache OFF$reset_color"; fi
	echo
}

gcc48() {
	export CC=gcc-4.8
	export CXX=g++-4.8
	whichcc
}

gcc49() {
	export CC=gcc-4.9
	export CXX=g++-4.9
	whichcc
}

gcclast() {
	unset CC
	unset CXX
	export CC
	export CXX
	whichcc
}

clanglast() {
	export CC=clang
	export CXX=clang++
	whichcc
}

rtagsoff() {
	ENABLE_RTAGS=0
	whichcc
}
rtagson() {
	ENABLE_RTAGS=1
	whichcc
}
ccacheoff() {
	ENABLE_CCACHE=0
	whichcc
}
ccacheon() {
	ENABLE_CCACHE=1
	whichcc
}

ENABLE_RTAGS=0
ENABLE_CCACHE=0
clanglast

#export CFLAGS="-Wno-undefined-var-template"
