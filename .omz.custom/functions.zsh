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

function bak() {
	for f in "$@"
	do
		cp $f{,.bak} || echo -e "\033[1;31mbak failed: $f\033[0;0m"
	done
}

function bakr() {
	for f in "$@"
	do
		cp -r $f{,.bak} || echo -e "\033[1;31mbak failed: $f\033[0;0m"
	done
}

function bakmv() {
	for f in "$@"
	do
		mv $f{,.bak} || echo -e "\033[1;31mbak failed: $f\033[0;0m"
	done
}

function unbak() {
	for f in "$@"
	do
		cp $f{.bak,}
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
