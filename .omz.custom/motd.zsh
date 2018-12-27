#!/bin/zsh

MOTD_FILE="$HOME/motd"

motd() {
	[[ -f "$MOTD_FILE" ]] || return 0

	zmodload zsh/mapfile
	lines=( "${(f@)mapfile[$MOTD_FILE]}" )
	[[ "${#lines[@]}" -gt 0 ]] || return 0

	echo
	for l in "${lines[@]}"
	do
		echo "    $l"
	done
	echo
}

motd
