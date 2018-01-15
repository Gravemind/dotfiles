#!/bin/zsh

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

##
## Screen
##

nvidia-settings --load-config-only
#xrandr --output DVI-I-1  --auto --primary --right-of DVI-D-0
xrandr --dpi 94
#export __GL_THREADED_OPTIMIZATIONS=1
#export __GL_YIELD="USLEEP"
#export __GL_SYNC_DISPLAY_DEVICE=DP-4
#export VDPAU_NVIDIA_SYNC_DISPLAY_DEVICE=DP-4
# sleep
#xset s 3600 3600
#xset dmps 3600 3600 3600

##
## Keyboard
##

setxkbmap -option terminate:ctrl_alt_bksp
## desactivate touchpad when typing
#syndaemon -i 2 -t -k -d &
setxkbmap us_qwerty-fr
# 'Menu' key as Windows key
#xmodmap -e 'keycode 135 = Super_R'
numlockx on

##
## Sound
##

# set volume
pactl set-sink-volume @DEFAULT_SINK@ 50%
# no bell
xset b 0 &> /dev/null

##
## Start
##

## ssh agent
eval "$(ssh-agent)"

mv ~/.i3/log ~/.i3/log.1
exec i3 >& ~/.i3/log
