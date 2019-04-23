#!/bin/bash

here="$(dirname "$0")"
base="${0##*/}"

log="$here/logdevmon"

notify="notify-send -a daemon_devmon -h string:synchronous:%fdevmon"

mv "$log" "$log.bak"

args=(
    devmon
    --sync # sync of flush mount option
    #--info-on-mount
    --exec-on-drive
    "$notify"' "Mounted "%f %d" ("%l")" -i media-removable-symbolic.symbolic'
    --exec-on-remove
    #"$notify"' "Removed "%f %d" ("%l")" -i edit-delete-symbolic.symbolic'
    "$notify"' "Removed "%f -i edit-delete-symbolic.symbolic'
)

if [[ -t 1 ]]; then # isatty
    "${args[@]}" |& tee "$log"
else
    "${args[@]}" >& "$log"
fi
