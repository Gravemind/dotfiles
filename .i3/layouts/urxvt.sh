#!/bin/bash

here="$(dirname "$(readlink -f "$0")")"

ws="${1:-}"
name="${2:-}"

tmp=`mktemp`
sed "s/{{NAME}}/$NAME/g" $here/urxvt.json.tpl > $tmp

$here/append_layout_window.sh $tmp $ws

rm $tmp

shift 2
if [ -z "$*" ]
then
	urxvt -name "$NAME" &
	disown
else
	urxvt -name "$NAME" -e zsh -is eval "$@" &
	disown
fi
