#!/bin/bash

here="${0%/*}"
workspace=
class=
instance=

usage() {
	echo "usage: $0 [-w WORKSPACE] [-c WIN_CLASS] [-i WIN_INSTANCE] [--] command [args...]"
	echo "	Spawns a i3 placeholder for command \"command\""
	echo "	  -w WORKSPACE		: move placeholder to workspace WORKSPACE (default: current)"
	echo "	  -c WIN_CLASS		: placeholder window's \"class\" to match (default: command)"
	echo "	  -i WIN_INSTANCE	: placeholder window's \"instance\" to match (no default)"
}
options=`getopt -o "+hfw:c:i:" -n "$0" -- "$@"`
eval set -- "$options"
while true; do
	case "$1" in
		-h) usage; exit 0 ;;
		-w) workspace="$2"; shift ;;
		-c) class="$2"; shift ;;
		-i) instance="$2"; shift ;;
		--) shift; break ;;
		*) break ;; ## no flags after first non-flag arg
	esac
	shift
done

if [ "$#" -lt 1 ]; then
	echo "$0: not enough arguments"
	usage
	exit 1
fi

prog="$1"
shift
prog_args=("$@")

name="$prog"
if [[ -z "$class" ]]; then
	class="${prog##*/}"
fi
if [[ -n "$instance" ]]; then
	name="$name $instance"
	instance_line=", \"instance\": \"(?i)^${instance}\$\""
else
	instance_line=
fi

tmp=`mktemp`
trap "{ rm -f $tmp; }" EXIT

layout_win_name="[[ ${name} ]]"

sponge $tmp <<EOF
{ "name": "$layout_win_name",
  "swallows": [ {
	"class": "(?i)^${class}\$"
	${instance_line}
  } ],
  "type": "con"
}
EOF

echo "$0: launching \"$prog\" class \"$class\" \"$instance\""

$here/append_layout_window.sh $tmp $workspace > /dev/null

"$prog" "${prog_args[@]}"

res=$?

## command failed !? close the placeholder if necessary
if [[ $res -ne 0 ]]
then
	winid="$(xwininfo -name "$layout_win_name" -int 2> /dev/null | sed -nE 's/.*Window id: ([0-9]+) .*/\1/gp')"
	if [[ -n "$winid" ]]
	then
		echo "$0: $prog exited $res (closing placeholder $winid)"
		xkill -id "$winid" > /dev/null
	else
		echo "$0: $prog exited $res (no more placeholder)"
	fi
else
	echo "$0: $prog exited $res"
fi

exit $res
