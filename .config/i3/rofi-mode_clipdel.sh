#!/bin/bash

export CM_LAUNCHER=rofi-script

if ! (( $# )); then
    clipmenu
    exit
else
    # https://github.com/koalaman/shellcheck/issues/1141
    # shellcheck disable=SC2124
    chosen_line="${@: -1}"

    res="$chosen_line"

    # https://stackoverflow.com/questions/407523/escape-a-string-for-a-sed-replace-pattern
    escaped="$(printf '%s' "$res" | sed 's/[]$*.^\\[]/\\&/g')"

    pattern="^${escaped}\$"
    #local pattern="$escaped"
    #echo "$0: clipdel pattern: $pattern"

    n="$(clipdel "$pattern" | wc -l)"
    if [[ $n -ne 1 ]]
    then
        msg="aborting because we matched $n entries instead of 1: $pattern"
        echo "$0: internal error: $msg"
        #res="$( { echo "INTERNAL ERROR:"; echo "$msg"; } | "${menu_cmd[@]}")"
        exit
    fi

    clipdel -d "$pattern"

    # re-run
    clipmenu
    exit
fi

