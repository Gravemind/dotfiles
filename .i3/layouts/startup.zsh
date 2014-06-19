#!/bin/zsh

HERE="$(cd "`dirname "$0"`"; pwd)"

$HERE/urxvt.zsh 1 main

$HERE/pidgin.zsh 9

$HERE/firefox.zsh 10

i3-msg workspace 1
i3-msg workspace 10
