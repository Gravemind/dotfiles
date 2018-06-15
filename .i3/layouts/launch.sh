#!/bin/bash

here="${0%/*}"
workspace=
class=
instance=

usage() {
	echo "usage: $0 [-w WORKSPACE] [-c WIN_CLASS] [-i WIN_INSTANCE] [--] command [args...]"
	echo "	Spawns a i3 placeholder for command \"command\""
	echo "	  -w WORKSPACE    : move placeholder to workspace WORKSPACE (default: current)"
	echo "	  -c WIN_CLASS    : placeholder window's \"class\" to match (default: command)"
	echo "	  -i WIN_INSTANCE : placeholder window's \"instance\" to match (no default)"
	echo "	  -q              : be quiet about it"
}
options=`getopt -o "+hqw:c:i:" -n "$0" -- "$@"`
[[ $? -eq 0 ]] || exit 1
eval set -- "$options"
quiet=0
while true; do
	case "$1" in
		-h) usage; exit 0 ;;
		-w) workspace="$2"; shift ;;
		-c) class="$2"; shift ;;
		-i) instance="$2"; shift ;;
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

tmp="$(mktemp -t "i3layoutlaunch.XXXXXXXX")"
#trap "{ rm -f $tmp; }" EXIT # removed in tmpexec script
tmpexec="$(mktemp -t "i3layoutlaunch.exec.XXXXXXXX")"
#trap "{ rm -f $tmpexec; }" EXIT # removed in tmpexec script

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

exec_from_i3msg=0
if [[ -n "$workspace" ]] ; then
	exec_from_i3msg=1
fi

log "launching \"$prog\" class \"$class\" instance \"$instance\""

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
echo "trap \"{ rm -f $tmp; }\" EXIT" >> $tmpexec
echo "trap \"{ rm -f $tmpexec; }\" EXIT" >> $tmpexec
echo "${prog_cmd[*]}" >> $tmpexec
#cat -e $tmpexec

# append_layout.sh hard coded so we can add the exec in the same i3-msg
msg=()
if [[ -n "$workspace" ]] ; then
	msg+=( "workspace $workspace;" )
fi
msg+=(
	"workspace tmpappendlayout;"
	"append_layout $tmp;" # ! no space between $tmp and ';'
	"focus child;"
	"move window to workspace back_and_forth;"
	"workspace back_and_forth;"
)

msg+=( "exec bash $tmpexec;" )

res="$(i3-msg "${msg[*]}")"
exi=$?

if [[ $exi -ne 0 || "$res" =~ false ]]
then
	log "error: i3-msg $msg"
	log "error: exit $exit: $res"
fi
