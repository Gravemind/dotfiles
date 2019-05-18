#!/bin/bash

export CM_LAUNCHER=rofi-script

#set -x

sanitize() {
    cat -v | \
        sed -E 's/[^a-zA-Z0-9\.-]+/_/g' | \
        #sed -E 's/_?-_?/-/g' | \
        sed -E 's/^[_-]+//g' | \
        sed -E 's/[_-]+$//g'
}

if ! (( $# )); then
    clipmenu | sanitize
    exit
else
    # https://github.com/koalaman/shellcheck/issues/1141
    # shellcheck disable=SC2124
    chosen_line="$*"

    res="$chosen_line"

    echo -n "$res" | xsel --logfile /dev/null -i --primary
    # BUG: xclip freezes !? logging "Waiting for selection request number "...
    #echo -n "$res" | xclip -i -verbose >&2 &
    #disown
fi
