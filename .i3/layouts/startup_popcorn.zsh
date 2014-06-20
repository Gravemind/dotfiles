#!/bin/zsh

HERE="$(cd "`dirname "$0"`"; pwd)"

$HERE/pidgin.zsh 9

$HERE/firefox.zsh 10

$HERE/startup_popcorn.zsh

i3-msg workspace 1
i3-msg workspace 10
