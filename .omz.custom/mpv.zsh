#!/bin/zsh

alias mpw="mpv --force-window=immediate "
alias mpa="mpv --no-video "

getfirstvalidfile() {
	while read file
	do
		# file=$(echo "$file")
		if [[ ! -e "$file" ]]
		then
			file="${(Q)file}"
			if [[ ! -e "$file" ]]
			then
				#echo "!! INVALID \"$file\"" >&2
				continue
			fi
		fi
		mime=$(file -b --mime-type	"$file" | grep -q 'video\|audio\|directory')
		if [[ $? == 0 ]]
		then
			echo "$file"
			return 1
		fi
	done
	return 0
}

mpn() {
	echo
	FOUNDFILE=$(
		history | grep mpv | \
			sort -r | \
			# extract mpv's entire parameters, and also print just the filename if found "/" \
			awk '/\s*[0-9]+\s+mpv\s+(.*)\s*/ { $1=""; $2=""; $0=$0; $1=$1; print $0; } /\// { sub(/.*\//,""); print $0; }' | \
			#sed -rn 's/\s*[0-9]+\s+mpv\s+(.*)/\1/gp' | \
			getfirstvalidfile
			 )
	if [[ -z "$FOUNDFILE" ]]
	then
		echo "No files in history have been found here"
		echo
		return 1
	fi

	NEXTFILE=$(\ls | sort -fiV | \grep -F "$FOUNDFILE" -A 100 | tail -n '+2' | getfirstvalidfile)

	if [[ ! -e "$NEXTFILE" ]]
	then
		echo "Did not found a next one after \"$FOUNDFILE\""
		echo
		return 1
	fi

	echo "  Last played: \"$FOUNDFILE\""
	echo "  Now playing: \"$NEXTFILE\""
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
