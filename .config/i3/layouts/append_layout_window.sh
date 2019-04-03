#!/bin/bash

layout="$1"
ws="${2:-}"
if [[ -z "$layout" || ! -f "$layout" ]]
then
	echo $0: no such layout "\"$layout\""
	echo usage: $0 layout [force_ws]
	exit 1
fi

msg=""
if [[ -n "$ws" ]]
then
    msg+="workspace $ws; "
fi
msg+="workspace tmpappend; append_layout $layout; focus child; move window to workspace back_and_forth; workspace back_and_forth"

res="$(i3-msg "$msg")"
exi=$?

if [[ $exi -ne 0 || "$res" =~ false ]]
then
    echo "$0: error: i3-msg $msg" 1>&2
    echo "$0: error: exit $exit: $res" 1>&2
fi
