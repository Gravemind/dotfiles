#!/bin/bash

here="$(dirname "$0")"

_rofi_greenclip-clear() {
    clear_msg="CLEAR"
    if ! (( $# )); then
        echo "$clear_msg"
        exit
    else
        chosen_line="$*"
        if [[ "$chosen_line" = "$clear_msg" ]]; then
            greenclip clear
        fi
    fi
}
export -f _rofi_greenclip-clear

if command -v rofi &> /dev/null; then
    cmd=(
        rofi
        -modi " clipboard:greenclip print, CLEAR:bash-call _rofi_greenclip-clear"
        -show ' clipboard'
    )
    "${cmd[@]}"

elif command -v dmenu &> /dev/null; then
    greenclip print | sed '/^$/d' | dmenu -i -l 10 -p clipboard | xargs -r -d'\n' -I '{}' greenclip print '{}'

fi
