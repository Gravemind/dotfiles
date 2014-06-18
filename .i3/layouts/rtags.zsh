#!/bin/zsh

HERE="$(cd "`dirname "$0"`"; pwd)"
$HERE/urxvt.zsh 6 "rtags" 'cd ~/bin/rtags;'
