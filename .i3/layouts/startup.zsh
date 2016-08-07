#!/bin/zsh

## unset option that nices background jobs
unsetopt BG_NICE

HERE="$(cd "`dirname "$0"`"; pwd)"

$HERE/urxvt.zsh 1 main

# $HERE/pidgin.zsh 9

$HERE/palemoon.zsh 10

i3-msg workspace 1
i3-msg workspace 10
