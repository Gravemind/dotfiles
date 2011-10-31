#!/bin/zsh

function wineprefix() {
    export WINEARCH=win32
    export WINEPREFIX=~/wineprefixes/win32
}

function sx() {
    cd $HOME
    startx &
    disown
    exit
}
