#!/bin/bash

here="${0%/*}"

usage() {
	echo 'usage: '"$0"' [-hq] [-w WORKSPACE] [-c WIN_CLASS] [-i WIN_INSTANCE] [-f WxH[+X+Y]] [--] command [args...]

    Spawns `command args...` with a i3 placeholder
      -w WORKSPACE    : move placeholder to workspace WORKSPACE (default: current)
      -c WIN_CLASS    : placeholder window'"'"'s "class" to match (default: command)
      -i WIN_INSTANCE : placeholder window'"'"'s "instance" to match (no default)
      -f WxH[+X+Y]    : make it floating, the geomerty is required.
                        negative X or Y positions the window from right or bottom
      -q              : be quiet about it
'
}

options=`getopt -o "+hqw:c:i:f:" -n "$0" -- "$@"`
[[ $? -eq 0 ]] || exit 1
eval set -- "$options"
quiet=0
workspace=
class=
instance=
floating=
geometry=
while true; do
	case "$1" in
		-h) usage; exit 0 ;;
		-w) workspace="$2"; shift ;;
		-c) class="$2"; shift ;;
		-i) instance="$2"; shift ;;
		-f) floating=1; geometry="$2"; shift; ;;
		-q) quiet=1; ;;
		--) shift; break ;;
		*) break ;; ## no flags after first non-flag arg
	esac
	shift
done

log() {
	[[ $quiet -eq 0 ]] || return;
	echo "$0:" "$@"
}

quote_arg() {
	printf '%q' "$*"
}

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

# tmpexec will self-destroy with tmplayout
tmplayout="$(mktemp -t "i3layoutlaunch_layout.XXXXXXXX")"
tmpexec="$(mktemp -t "i3layoutlaunch_exec.XXXXXXXX")"

layout_win_name="${name} ..."

layout_rect=""
if [[ -n "$geometry" ]]
then
	read screenw screenh < <(i3-msg -t get_workspaces | jq 'map(select(.focused))[0].rect | .width, .height' | tr $'\n' ' ')
	if [[ "$geometry" =~ ^([0-9]+)x([0-9]+)\+([0-9]+)\+([0-9]+)$ ]]
	then
		w="${BASH_REMATCH[1]}"
		h="${BASH_REMATCH[2]}"
		x="${BASH_REMATCH[3]}"
		y="${BASH_REMATCH[4]}"
		[[ $x -ge 0 ]] || x=$(( screenw + x ))
		[[ $y -ge 0 ]] || y=$(( screenh + y ))
	elif [[ "$geometry" =~ ^([0-9]+)x([0-9]+)$ ]]
	then
		w="${BASH_REMATCH[1]}"
		h="${BASH_REMATCH[2]}"
		# center
		x=$(( screenw / 2 - w / 2 ))
		y=$(( screenh / 2 - h / 2 ))
	else
		log "error: invalid geometry"
		exit 1
	fi
	layout_rect='
"rect": {
	"height": '"$h"',
	"width": '"$w"',
	"x": '"$x"',
	"y": '"$y"'
},'
fi

node='"name": "'"$layout_win_name"'",
"swallows": [ {
	"class": "(?i)^'"$class"'$"
	'"$instance_line"'
} ]'

if [[ "$floating" = 1 ]]
then
	echo '{
	"type": "floating_con", '"$layout_rect"'
	"nodes": [ {
		"floating": "user_on",
		'"$node"'
	} ]
}' > $tmplayout
else
	echo '{
	'"$node"'
}' > $tmplayout
fi

#cat $tmplayout

#
# All that so i3-msg execs the program with proper startup-up on the right
# workspace
#
# And had to have a tmpexec script because I could not get quoting working
# properly in i3-msg, e.g. `launch.sh -c urxvt bash -c 'sleep 1 ; urxvt'`
#

prog_cmd=()
prog_cmd+=( "$(quote_arg "$prog")" )
for arg in "${prog_args[@]}" ; do
	prog_cmd+=( "$(quote_arg "$arg")" )
done
declare -px  >> $tmpexec
printf "cd %q\n" "$(pwd)" >> $tmpexec
echo "trap \"{ rm -f $tmplayout; rm -f $tmpexec; }\" EXIT" >> $tmpexec
echo "${prog_cmd[*]}" >> $tmpexec
#cat -e $tmpexec

msg=()
[[ -z "$workspace" ]] || msg+=( "workspace $workspace" )
msg+=(
	"workspace launching"
	"append_layout $tmplayout"
	"focus child"
	"move window to workspace back_and_forth"
	"workspace back_and_forth"
)
msg+=( "exec bash $tmpexec" )

msgstr=$( ( IFS=$'\n'; echo "${msg[*]}" ) )

log "launching \"$prog\" class \"$class\" instance \"$instance\""
#echo "$msgstr"

res="$(i3-msg "$msgstr")"
exi=$?

if [[ $exi -ne 0 || "$res" =~ false ]]
then
	log "error: i3-msg $msg"
	log "error: exit $exit: $res"
# else
# 	log "no error"
fi
