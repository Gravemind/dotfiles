#!/bin/zsh

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

setxkbmap -option terminate:ctrl_alt_bksp

## desactivate touchpad when typing
#syndaemon -i 2 -t -k -d &

# xrandr --output DVI-I-1 --right-of VGA-1 

amixer sset Master 50% &> /dev/null
amixer sset Front 100% &> /dev/null
amixer sset PCM 100% &> /dev/null
amixer sset Mono 0% &> /dev/null

xset b 0 &> /dev/null
xset s 3600 3600
xset dmps 3600 3600 3600

setxkbmap us_qwerty-fr

## 'Menu' key as Windows key
# xmodmap -e 'keycode 135 = Super_R'

## ssh agent
ssh-agent -s > /tmp/ssh.keys
.  /tmp/ssh.keys
rm  /tmp/ssh.keys

numlockx on

mv ~/.i3/log ~/.i3/log.1
exec i3 >& ~/.i3/log
