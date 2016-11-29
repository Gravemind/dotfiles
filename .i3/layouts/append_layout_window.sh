#!/bin/bash

layout="$1"
ws="${2:-}"
if [[ -z "$layout" || ! -f "$layout" ]]
then
	echo no such layout "\"$layout\""
	echo usage: $0 layout [force_ws]
	exit 1
fi

force_ws=""
if [[ -n "$ws" ]]
then
	force_ws="workspace $ws; "
fi

i3-msg "$force_ws workspace tmpappend; append_layout $layout; focus child; move window to workspace back_and_forth; workspace back_and_forth"
