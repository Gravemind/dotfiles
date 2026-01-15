#!/bin/bash
# usage: exec_daemon <name> <command>...

set -uo pipefail

instance_name="$1"
shift
command=("$@")

instance_id="$DISPLAY"

log() { echo "exec_daemon $instance_name-$instance_id: $*" 1>&2; }
die() { log "fatal error: $*"; exit 1; }

gracefulkill() {
    [[ -n "$1" ]] || return 0
    if /bin/kill -0 "$1" 2>/dev/null
    then
        log "kill $1"
        /bin/kill --verbose --timeout 2000 KILL --signal TERM "$1" 2>/dev/null
    fi
}
export -f gracefulkill

piddir="${XDG_RUNTIME_DIR:-/run/user/$UID}"
pidfile="$piddir/$instance_name-$instance_id.pid"

pid="$$"

exec {lockfd}<>"$pidfile"
flock -x -w 10 "$lockfd" || die "flock failed"
read -r oldpid <"$pidfile"
echo "$pid" > "$pidfile"
exec {lockfd}>&-

if [[ "$oldpid" != "$pid" ]]
then
    gracefulkill "$oldpid"
fi

log "exec $(printf ' %q' "${command[@]}")"
exec "${command[@]}"
