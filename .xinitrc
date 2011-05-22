#!/bin/zsh

amixer sset Master 50% &> /dev/null
amixer sset Front 100% &> /dev/null
amixer sset PCM 100% &> /dev/null
amixer sset Mono 0% &> /dev/null

xset b 0 &> /dev/null

xrandr --output HDMI-0 --auto --right-of DVI-0

numlockx on

exec i3 > /home/jo/.i3/log
