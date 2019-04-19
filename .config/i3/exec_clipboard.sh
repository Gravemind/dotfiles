#!/bin/bash

export CM_LAUNCHER=rofi-script

here="$(dirname "$0")"

cmd=(
    rofi
    -i
    -show " copy"
    -modi " copy:clipmenu, delete:$here/rofi-mode_clipdel.sh, sanitized:$here/rofi-mode_clipsanitize.sh"
)
"${cmd[@]}"
