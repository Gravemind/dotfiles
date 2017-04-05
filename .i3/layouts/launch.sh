#!/bin/bash

here="${0%/*}"
workspace=
class=
instance=

usage() {
	echo "usage: $0 [-w WORKSPACE] [-c WIN_CLASS] [-i WIN_INSTANCE] -- command [args...]"
	echo "  Spawns a i3 placeholder for command \"command\""
	echo "    -w WORKSPACE	    : move placeholder to workspace WORKSPACE (default: current)"
	echo "    -c WIN_CLASS	    : placeholder window's \"class\" to match (default: command)"
	echo "    -i WIN_INSTANCE   : placeholder window's \"instance\" to match (no default)"
}
options=`getopt -o "+hw:c:i:" -n "$0" -- "$@"`
eval set -- "$options"
while true; do
	case "$1" in
		-h) usage; exit 0 ;;
		-w) workspace="$2"; shift ;;
		-c) class="$2"; shift ;;
		-i) instance="$2"; shift ;;
		--) shift; break ;;
		*) echo $0: invalid argument "$1"; exit 1;;
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

sponge $tmp <<EOF
{ "name": "[[ ${name} ]]",
  "swallows": [ {
	"class": "(?i)^${class}\$"
	${instance_line}
  } ],
  "type": "con"
}
EOF

echo $0: launching "$prog" class "$class" "$instance"

early_placeholder=$([[ -z "$workspace" ]] || echo true)
if [ "x$early_placeholder" = "xtrue" ] ; then
	$here/append_layout_window.sh $tmp $workspace
fi

"$prog" "${prog_args[@]}" &
prog_pid=$!

sleep 0.3
ps $prog_pid > /dev/null
if [[ $? -ne 0 ]]
then
	echo $0: program "$prog" not running !
	exit 1
else
	disown
	if [ "x$early_placeholder" != "xtrue" ] ; then
		xdotool search --pid $prog_pid > /dev/null
		if [[ $? -ne 0 ]]
		then
			$here/append_layout_window.sh $tmp $workspace
		else
			echo "$0: $prog window already opened"
		fi
	fi
fi
