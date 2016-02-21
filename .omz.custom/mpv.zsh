#!/bin/zsh

alias mp="mpv"

findnextmpv() {
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
		mime=$(file -b --mime-type  "$file" | grep -q 'video\|audio\|directory')
		if [[ $? == 0 ]]
		then
			echo "$file"
			return 1
		fi
	done
	return 0
}

mpn() {
	MAXFILES=1000

	takefirst() {
		echo "$1"
	}

	FOUNDFILE=$(
		history | \
			tail -n $MAXFILES | sort -r | \
			sed -rn 's/\s*[0-9]+\s+mpv\s+(.*)/\1/gp' | \
			findnextmpv
			 )
	if [[ -z "$FOUNDFILE" ]]
	then
		echo "No files in history have been found here"
		return 1
	fi
	echo
	echo "LAST ONE :: \"$FOUNDFILE\""

	NEXTFILE=$(\ls | sort -fi | \grep -F "$FOUNDFILE" -A 100 | tail -n '+2' | findnextmpv)
	echo "$NEXTFILE"
	if [[ ! -e "$NEXTFILE" ]]
	then
		echo "Did not found the next one after \"$FOUNDFILE\""
		return 1
	fi

	echo "NEXT ONE :: \"$NEXTFILE\""
	echo

	#return 0

	mpv "$NEXTFILE" && \
		print -s "mpv \"$NEXTFILE\""

}

mpf() {
	FIRST="$(ls | sort -fi | findnextmpv)"
	mpv "$FIRST" && \
		print -s "mpv \"$FIRST\""
}
