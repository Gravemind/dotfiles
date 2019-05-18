#!/bin/bash


here="$(dirname "$0")"

cmd=(
    rofi
    -i
    -show " nvidia" -modi
    " nvidia:$here/rofi-mode_nvidia_mode.sh"
)
"${cmd[@]}"

