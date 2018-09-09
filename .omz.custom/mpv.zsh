#!/bin/zsh

alias mpa="mpv --no-video "
alias mpw="mpv --force-window=immediate "

mpcb() {
	url="$(xclip -o -r)"
	echo -n "mpv $@ '$url' ? [y/n]"
	read -r -k1 r
	echo
	if [[ "$r" == "y" ]]
	then
		mpv --force-window=immediate "$@" "$url"
			 # && print -s "mpv $@ \"$url\""
	fi
}

getfirstvalidfile() {
	while IFS= read -r -d $'\n' file
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
	return 0
}

mpn() {
	echo

	FOUNDFILE="$( history | \
			grep mpv | \
			tac | \
			# extract mpv's entire parameters, and also print just the filename if found "/" \
			awk '/\s*[0-9]+\s+mpv\s+(.*)\s*/ { $1=""; $2=""; $0=$0; $1=$1; print $0; } /\// { sub(/.*\//,""); print $0; }' | \
			#sed -rn 's/\s*[0-9]+\s+mpv\s+(.*)/\1/gp' | \
			getfirstvalidfile )"

	if [[ -z "$FOUNDFILE" ]]
	then
		echo "No files in history have been found here"
		echo
		return 1
	fi

	echo "  Last played: \"$FOUNDFILE\""

	NEXTFILE="$(\ls | sort -fiV | \grep -F "$FOUNDFILE" -A 100 | tail -n '+2' | getfirstvalidfile)"

	if [[ ! -e "$NEXTFILE" ]]
	then
		echo "  Did not found a next one after that"
		echo
		return 1
	fi

	echo "  Now playing: \"$NEXTFILE\""
	echo
	echo "  $(wdiff -n <( echo "$FOUNDFILE" ) <( echo "$NEXTFILE" ) | colordiff)"
	echo

	#return 0

	mpv "$NEXTFILE" && \
		print -s "mpv \"$NEXTFILE\""
}

mpf() {
	FIRST="$(ls | sort -fiV | getfirstvalidfile)"
	echo
	echo "  Now playing: \"$FIRST\""
	echo
	mpv "$FIRST" && \
		print -s "mpv \"$FIRST\""
}
