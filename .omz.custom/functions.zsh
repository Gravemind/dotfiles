#!/bin/zsh

function wineprefix() {
	export WINEARCH=win32
	export WINEPREFIX=~/wineprefixes/win32
}

function sx() {
	cd $HOME
	startx &
	disown
	exit
}

function dolast() {
	LAST=`\ls -t | head -n 1`
	echo "\n## dolast" "$@" "$LAST\n" 1>&2
	"$@" "$LAST"
}

function mnt() {
	sudo mount -o gid=`id -g`,uid=`id -u` "$@"
}

function _rmrf_ask() {
	local f="$1"
	if [[ -e "$f" ]]
	then
		echo -n "Overwrite $f ? [y/n] "
		read -r -k1 r
		echo
		[[ "$r" = y ]] || { echo "aborted"; return 1 }
		rm -rf "$f"
	fi
}

function bak() {
	for f in "$@"
	do
		f="${f%.bak}"
		local bak="$f.bak"
		_rmrf_ask "$f.bak" || return 1
		cp -aT "$f" "$bak" || { echo -e "\033[1;31mbak failed: $f\033[0;0m" 1>&2 ; return 1; }
	done
}

function bakmv() {
	for f in "$@"
	do
		f="${f%.bak}"
		local bak="$f.bak"
		_rmrf_ask "$f.bak" || return 1
		mv -T "$f" "$bak" || { echo -e "\033[1;31mbak failed: $f\033[0;0m" 1>&2 ; return 1; }
	done
}

function unbak() {
	for f in "$@"
	do
		f="${f%.bak}"
		local bak="$f.bak"
		_rmrf_ask "$f" || return 1
		cp -aT "$bak" "$f" || { echo -e "\033[1;31mbak failed: $f\033[0;0m" 1>&2 ; return 1; }
	done
}

## $> tmp COMMAND
## $> COMMAND | tmp
## sponges COMMAND stdout to a temp file, then prints this temp file path
## example: git diff --no-index $(tmp ls a) $(tmp ls b)
function tmp() {
	local tmpfile="$(mktemp /tmp/tmptmpouput.XXXXXXXXXX)"
	if [[ "$#" -gt 0 ]]
	then
		"$@" > "$tmpfile"
	else
		sponge "$tmpfile"
	fi
	echo "$tmpfile"
}

## rm all temp files created by `tmp`
function cleantmp() {
	rm -f /tmp/tmptmpouput.*
}
