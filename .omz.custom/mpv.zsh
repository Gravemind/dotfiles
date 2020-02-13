#!/bin/zsh

alias mpa="mpv --no-video "
alias mpw="mpv --force-window=immediate "

GRAVEMIND_NO_URGENT_CMD+=( mpn mpf )

MPN_SORT_VER=0
MPN_MPV_ARGS= # --fullscreen

_mpn_sort_files() {
	if [[ "${MPN_SORT_VER}" = "1" ]]
	then
		sort -fiV
	else
		sort -fi
	fi
}

_mpn_grep_valid_files() {
	local first=0
	[[ "${1:-}" != "-1" ]] || first=1
	while read -r file
	do
		file="$(basename "$file")"
		#file="./$file"
		#echo "FILE $file" >&2
		# file=$(echo "$file")
		# if [[ "${file[0]}" = "/" ]]
		# then
		# 	continue
		# fi

		if [[ ! -e "$file" ]]
		then
			file="${(Q)file}"
			#echo "FILE $file" >&2
			if [[ ! -e "$file" ]]
			then
				#echo "!! INVALID \"$file\"" >&2
				continue
			fi
		fi

		# Allow directories
		if [[ -d "$file" ]]
		then
			echo "$file"
			[[ $first = 0 ]] || return 0
			continue
		fi

		# Allow by extension
		local ext="${file##*.}"
		if [[ "$ext" =~ mkv\|mka\|mp4\|webm\|avi ]]
		then
			echo "$file"
			[[ $first = 0 ]] || return 0
			continue
		fi

		# Allow by mime (slow)
		local mime="$(file -b --mime-type "$file")"
		if [[ "$mime" =~ video\|audio ]]
		then
			echo "_mpn_grep_valid_files: note: slow mime type used, consider matching the extension of $file" >&2
			echo "$file"
			[[ $first = 0 ]] || return 0
			continue
		fi
	done
	return 1
}

_mpn_last_valid_file_from_history() {
	while read -r n bin args
	do
		[[ "$bin" == "mpv" ]] || continue;
		echo "$args" | xargs -n1 echo | _mpn_grep_valid_files -1 && return 0
	done < <(history | grep mpv | tac)
}

mpn() {
	echo

	local last_file="$(_mpn_last_valid_file_from_history)"

	if [[ -z "$last_file" ]]
	then
		echo "No files in history have been found here"
		echo
		return 1
	fi

	echo "Last played: $last_file"
	echo

	local found=0
	local next_file=
	while read -r file
	do
		if [[ ! -e "$file" ]]
		then
			echo "Escaping error ? file not found: $file" >&2
			return 1
		fi
		if [[ $found = 0 ]]
		then
			if [[ "$file" != "$last_file" ]]
			then
				echo "  $file"
			else
				echo "- $file"
				found=1
			fi
		elif [[ $found = 1 ]]
		then
			 next_file="$file"

			 local common="$({ echo "$last_file"; echo "$next_file"; } | sed -e 'N;s/^\(.*\).*\n\1.*$/\1/')"
			 local len="${#common}"
			 #echo "last: ${common}"$'\e[31m'"${last_file:$len}"$'\e[0m'
			 echo "+ ${common}"$'\e[1;32m'"${next_file:$len}"$'\e[0;0m'

			 found=2
		else
			echo "  $file"
		fi
	done < <( \ls | _mpn_sort_files | _mpn_grep_valid_files )
	echo

	if [[ -z "$next_file" ]]
	then
		echo "Could not find next file after $last"
		echo
		return 1
	fi

	local cmd=( mpv $MPN_MPV_ARGS "$@" "$next_file")

	echo "Now playing: ${(q-)cmd[@]}"
	echo

	#return 0

	"${cmd[@]}" && \
		print -sf "%s " "${(q-)cmd[@]}"
}

mpf() {
	local first="$(\ls | _mpn_sort_files | _mpn_grep_valid_files -1)"

	local cmd=( mpv $MPN_MPV_ARGS "$@" "$first")

	echo "Now playing: ${(q-)cmd[@]}"
	echo

	#return 0

	"${cmd[@]}" && \
		print -sf "%s " "${(q-)cmd[@]}"
}

mploop() {
	next=y
	while [[ "$next" = y ]]
	do
		mpn "$@" || return 1
		read -r -k 1 'next? next ? [y] '
		echo $next
	done
}
