#!/bin/zsh

## wait 10 seconds for 'rm *'.
## (force stop wait with Ctrl-C, or expand the '*' with TAB)
setopt rm_star_wait

function rmstar() {
	local args=("${@:-.}")
	local dirs=()
	for dir in "${args[@]}"; do
		echo
		if [[ ! -d "$dir" ]]; then
			echo "$dir: ${fg_bold[red]}not a directory, ignoring$reset_color"
			continue
		fi
		dir="$(readlink -f "$dir")"
		echo "$dir:"
		echo ""
		#echo "${fg_bold[red]}$dir:$reset_color"
		ls -A1F "$dir"
		dirs+=("$dir")
	done
	echo

	[[ "${#dirs}" -gt 0 ]] || return 1;

	echo "${fg_bold[red]}Delete content of ${dirs[@]}"
	echo -n "[yes] ? $reset_color"
	read -r r
	if [[ "$r" != yes ]]; then
		echo abort
		return 1;
	fi
	echo
	for dir in "${dirs[@]}"; do
		echo -n "$dir/* ... "
		# `find -delete` is supposedly faster than `rm -rf` for networked fs
		find "$dir" -mindepth 1 -delete && echo -n "deleted." || echo -n "FAILED!"
		echo
	done
}

alias rmallin=rmstar
