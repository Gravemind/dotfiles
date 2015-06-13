#!/bin/zsh

alias mp="mpv"

mpn() {
	MAXFILES=1000

	takefirst() {
		echo "$1"
	}

	FOUNDFILE=$(
		history | \
			tail -n $MAXFILES | sort -r | \
			sed -rn 's/\s*[0-9]+\s+mpv\s+(.*)/\1/gp' | \
			while read file
			do
				file=${(Q)file}
				if [[ -f "$file" ]]
				then
					echo $file
					break
				fi
			done
		)

	if [[ -z "$FOUNDFILE" ]]
	then
		echo "No files in history have been found here"
		return 1
	fi

	BOTH=$(ls | sort | grep "$FOUNDFILE" -A 1)
	COUNT=$(echo "$BOTH" | wc -l)

	if [[ $COUNT -ne 2 ]]
	then
		echo "Did not found the next one after \"$FOUNDFILE\""
		return 1
	fi

	NEXTFILE=$(echo "$BOTH" | tail -n 1)

	echo
	echo "mpv \"$NEXTFILE\""
	echo

	#return 0

	mpv "$NEXTFILE" && \
		print -s "mpv \"$NEXTFILE\""

}

mpf() {
	FIRST="$(ls | sort | head -n 1)"
	mpv "$FIRST" && \
		print -s "mpv \"$FIRST\""
}
