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

_mpn_get_first_valid_file() {
	while read -r file
	do
		#echo "FILE $file" >&2
		# file=$(echo "$file")
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
		ext="${file##*.}"
		mime="$(file -b --mime-type	"$file")"
		# echo "FILE $file $mime $ext" >&2
		# mkv mime type sometimes does not report as video/audio
		if [[ "$mime" =~ video\|audio\|directory || "$ext" =~ mkv\|mka\|mp4\|webm\|avi ]]
		then
			echo "$file"
			return 0
		fi
		#echo "W? $file"
	done
	return 1
}

_mpn_last_valid_file_from_history() {
	while read -r n bin args
	do
		[[ "$bin" == "mpv" ]] || continue;
		echo "$args" | xargs -n1 echo | _mpn_get_first_valid_file && return 0
	done < <(history | grep mpv | tac)
}

mpn() {
	echo

	FOUNDFILE="$(_mpn_last_valid_file_from_history)"

	if [[ -z "$FOUNDFILE" ]]
	then
		echo "No files in history have been found here"
		echo
		return 1
	fi

	echo "  Last played: \"$FOUNDFILE\""

	NEXTFILE="$(\ls | _mpn_sort_files | \grep -F "$FOUNDFILE" -A 100 | tail -n '+2' | _mpn_get_first_valid_file)"

	if [[ ! -e "$NEXTFILE" ]]
	then
		echo "  Did not found a next one after that"
		echo
		return 1
	fi

	echo "  Now playing: \"$NEXTFILE\""

	echo
	local common="$({ echo "$FOUNDFILE"; echo "$NEXTFILE"; } | sed -e 'N;s/^\(.*\).*\n\1.*$/\1/')"
	local len="${#common}"
	echo "  ${common}"$'\e[31m'"${FOUNDFILE:$len}"$'\e[0m'
	echo "  ${common}"$'\e[32m'"${NEXTFILE:$len}"$'\e[0m'
	echo

	# return 0

	local args=( $MPN_MPV_ARGS "$@" -- "$NEXTFILE")

	mpv "${args[@]}" && \
		print -s "mpv" "${(q-)args[@]}" # print -s is not printf-like
}

mpf() {
	FIRST="$(\ls | _mpn_sort_files | _mpn_get_first_valid_file)"
	echo
	echo "  Now playing: \"$FIRST\""
	echo
	mpv "$FIRST" && \
		print -s "mpv \"$FIRST\""
}
