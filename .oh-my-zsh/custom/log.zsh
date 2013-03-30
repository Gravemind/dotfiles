#!/bin/zsh

function log() {
	local f
	f="/var/log/$1"
	if [[ -f $f ]]
	then
		if [[ -r $f ]]
		then
			less -S +G "$f"
		else
			sudo less -S +G "$f"
		fi
	else
		echo "$0: cannot access $f: No such file"
	fi
}

_log_files_list_files() {
	local ret=1
	cd /var/log/
	_files
}

_log_files() {
	_log_files_list_files
}

compdef _log_files log
