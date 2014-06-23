#!/bin/zsh

HERE="$(cd "`dirname "$0"`"; pwd)"

WS="$1"
if [ -z "$WS" ]
then
	WS=9
fi

i3-msg "workspace $WS; append_layout $HERE/pidgin.json"
pidgin &
