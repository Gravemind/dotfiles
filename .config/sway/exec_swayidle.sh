#!/bin/bash

set -euo pipefail

killall --wait --exact swayidle ||:

cmd=(
    swayidle -w

    # When swaylock is detected, sleep after 10s
    timeout 10 'if pgrep -x swaylock; then swaymsg "output * power off"; fi'
    resume 'swaymsg "output * power on"'

    timeout $((10 * 60)) 'swaymsg "output * power off"'
    resume 'swaymsg "output * power on"'
)
exec "${cmd[@]}"
