#!/bin/zsh

HERE="$(cd "`dirname "$0"`"; pwd)"

WS="$1"
NAME="$2"

TMP=`mktemp`

sed "s/{{NAME}}/$NAME/g" $HERE/urxvt.json.tpl > $TMP

i3-msg "workspace $WS; append_layout $TMP;"

rm $TMP

shift 2

CMD="$@"
if [ -z "$CMD" ]
then
	urxvt -name "$NAME" &
else
	urxvt -name "$NAME" -e zsh -is eval "$@" &
fi
