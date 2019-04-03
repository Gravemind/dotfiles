#!/bin/bash

#dmenu_run -b -p ">>" -nb '#222222' -nf '#CCCCCC' -sb '#4c7899' -sf '#FFFFFF'
#rofi -show run
#rofi -show combi -combi-modi 'run,ssh,window' -modi combi

here="$(dirname "$0")"

cmd=(
    rofi -show combi
    -display-combi ' launch'
    -modi combi
    -combi-modi "drun,flatpak:$here/rofi-mode_flatpak.sh,run,firejail:$here/rofi-mode_firejail.sh,window"
    -drun-match-fields name,exec
    -drun-display-format '{name} <span size="small" alpha="80%">{exec}</span>'
    # -drun-show-actions
)
"${cmd[@]}"
