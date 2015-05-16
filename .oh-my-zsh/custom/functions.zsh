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

function agent() {
	ssh-agent -s > /tmp/ssh.keys
	. /tmp/ssh.keys
	rm /tmp/ssh.keys
	ssh-add
}

function mnt() {
	sudo mount -o gid=`id -g`,uid=`id -u` "$@"
}

function bak() {
	for f in "$@"
	do
		cp $f{,.bak}
	done
}

function sbak() {
	for f in "$@"
	do
		sudo cp $f{,.bak}
	done
}
