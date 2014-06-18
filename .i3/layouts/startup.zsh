#!/bin/zsh

HERE="$(cd "`dirname "$0"`"; pwd)"

$HERE/urxvt.zsh 1 main

i3-msg "workspace 9; append_layout $HERE/pidgin.json"
pidgin & ; disown

i3-msg "workspace 10; append_layout $HERE/firefox.json"
firefox & ; disown

i3-msg workspace 1
i3-msg workspace 10
