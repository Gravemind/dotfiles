#!/bin/zsh

## unset option that nices background jobs
unsetopt BG_NICE

HERE="$(cd "`dirname "$0"`"; pwd)"

WS="$1"
if [ -z "$WS" ]
then
	WS=10
fi

i3-msg "workspace $WS; append_layout $HERE/palemoon.json"
palemoon &
disown
