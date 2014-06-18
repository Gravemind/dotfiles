#!/bin/zsh

HERE="`dirname \"$0\"`"

WS="$1"
NAME="$2"

TMP=`mktemp`

sed "s/{{NAME}}/$NAME/g" $HERE/_urxvt_named.json > $TMP

i3-msg "workspace $WS; append_layout $TMP;"

rm $TMP

shift 2

CMD="$@"
if [ -z "$CMD" ]
then
	urxvt -name "$NAME" & ; disown
else
	urxvt -name "$NAME" -e zsh -is eval "$@" & ; disown
fi
