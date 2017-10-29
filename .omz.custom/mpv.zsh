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
	FOUNDFILE=$(
		history | grep mpv | \
			sort -r | \
			sed -rn 's/\s*[0-9]+\s+mpv\s+(.*)/\1/gp' | \
			getfirstvalidfile
			 )
	if [[ -z "$FOUNDFILE" ]]
	then
		echo "No files in history have been found here"
		return 1
	fi
	echo
	echo "( Last played: \"$FOUNDFILE\" )"

	NEXTFILE=$(\ls | sort -fi | \grep -F "$FOUNDFILE" -A 100 | tail -n '+2' | getfirstvalidfile)

	if [[ ! -e "$NEXTFILE" ]]
	then
		echo
		echo "Did not found a next one after \"$FOUNDFILE\""
		echo
		return 1
	fi

	echo
	echo "Playing \"$NEXTFILE\""
	echo

	#return 0

	mpv "$NEXTFILE" && \
		print -s "mpv \"$NEXTFILE\""
}

mpf() {
	FIRST="$(ls | sort -fi | getfirstvalidfile)"
	echo
	echo "Playing \"$FIRST\""
	echo
	mpv "$FIRST" && \
		print -s "mpv \"$FIRST\""
}
