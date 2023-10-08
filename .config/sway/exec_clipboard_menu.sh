#!/bin/bash

# clipman pick -t rofi && {
#     wl-paste -n | wl-copy --primary
# }

# cliphist list | rofi -dmenu | cliphist decode | pee 'wl-copy' 'wl-copy --primary'

here="$(dirname "$0")"
cmd=(
    rofi
    -i
    -show " cliphist"
    -modi " cliphist:$here/rofi-mod_cliphist.sh, delete:$here/rofi-mod_cliphist-delete.sh"
)
"${cmd[@]}"
