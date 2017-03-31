#!/bin/zsh

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

xrandr --setprovideroutputsource modesetting NVIDIA-0
# xrandr --setprovideroutputsource Intel
xrandr --auto

nvidia-settings --load-config-only &
# xrandr --output DVI-I-1  --auto --primary --right-of DVI-D-0
xrandr --dpi 96

export __GL_YIELD="USLEEP"

setxkbmap -option terminate:ctrl_alt_bksp

## desactivate touchpad when typing
#syndaemon -i 2 -t -k -d &

pactl set-sink-volume @DEFAULT_SINK@ 50%

xset b 0 &> /dev/null
#xset s 3600 3600
#xset dmps 3600 3600 3600

setxkbmap us_qwerty-fr

## 'Menu' key as Windows key
xmodmap -e 'keycode 135 = Super_R'

## ssh agent
eval "$(ssh-agent)"

# numlockx on

mv ~/.i3/log ~/.i3/log.1
exec i3 >& ~/.i3/log
