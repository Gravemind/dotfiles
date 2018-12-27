
TODOFILE="$HOME/TODO.org"

todo() {
	if [ -n "$1" ]
	then
		echo "$@" >> "$TODOFILE"
	fi
	echo " ┌─────"
	\cat "$TODOFILE" | sed -r '/^\s*$/d' | sed 's/.*/ │ &/'
	echo " └"
}

netodo() {
	ne "$TODOFILE"
}

# todo;
