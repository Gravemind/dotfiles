#!/bin/zsh

## desactivate touchpad when typing
#syndaemon -i 2 -t -k -d &

# xrandr --output DVI-I-1 --right-of VGA-1 

amixer sset Master 50% &> /dev/null
amixer sset Front 100% &> /dev/null
amixer sset PCM 100% &> /dev/null
amixer sset Mono 0% &> /dev/null

xset b 0 &> /dev/null

setxkbmap us_qwerty-fr

## 'Menu' key as Windows key
# xmodmap -e 'keycode 135 = Super_R'

## ssh agent
ssh-agent -s > /tmp/ssh.keys
.  /tmp/ssh.keys
rm  /tmp/ssh.keys

numlockx on

exec i3 &> /home/jo/.i3/log
