#!/bin/bash

here="$(dirname "$0")"

cmd=(
    rofi
    -i
    -show " sway" -modi
    " sway:$here/rofi-mod_sway-output-mode.sh"
)
"${cmd[@]}"
