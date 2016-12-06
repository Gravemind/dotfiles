#!/bin/zsh

ENABLE_RTAGS=0
ENABLE_CCACHE=0

whichcc() {
	_pathaddorremove "$ENABLE_CCACHE" "$HOME/bin/ccache_bin"
	_pathaddorremove "$ENABLE_RTAGS" "$HOME/bin/rtags_bin"
	rehash

	## echo gcc g++ $(g++ --version | sed -nr 's/[^0-9.]*([0-9]+(\.[0-9]+)+).*/\1/p') $(which gcc g++)
	## echo clang clang++ $(clang++ --version | sed -nr 's/[^0-9.]*([0-9]+(\.[0-9]+)+).*/\1/p') $(which clang clang++)
	local cc="${CC:-cc}"
	echo -n "CC: $cc, "
	local allcc=$(which -a $cc)
	if [[ "$allcc" == *rtags_bin* ]]; then echo -n "$fg_bold[green]rtags ON$reset_color"; else; echo -n "$fg_bold[black]rtags OFF$reset_color"; fi
	echo -n ', '
	if [[ "$allcc" == *ccache_bin* ]]; then echo -n "$fg_bold[green]ccache ON$reset_color"; else; echo -n "$fg_bold[black]cache OFF$reset_color"; fi
	echo
}

_pathaddorremove() {
	local enable="$1"
	local mypath="$2"
	export PATH=$(echo "$PATH" | sed "s#${mypath}:##g")
	if [[ "$enable" = "1" ]]
	then
		export PATH="${mypath}:$PATH"
	fi
}

gcc48() {
	export CC=gcc-4.8
	export CXX=g++-4.8
	export CPP=$CXX
	whichcc
}

gcc49() {
	export CC=gcc-4.9
	export CXX=g++-4.9
	export CPP=$CXX
	whichcc
}

gcclast() {
	unset CC
	unset CXX
	unset CPP
	export CC
	export CXX
	export CPP
	whichcc
}

clanglast() {
	export CC=clang
	export CXX=clang++
	export CPP=$CXX
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
gcclast
