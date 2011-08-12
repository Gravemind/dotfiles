#!/bin/zsh

sub wineprefix()
{
    export WINEARCH=win32
    export WINEPREFIX=~/wineprefixes/win32
}

sub sx()
{
    cd $HOME
    startx &
    disown
    exit
}
