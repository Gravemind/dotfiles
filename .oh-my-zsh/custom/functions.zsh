#!/bin/zsh

sub wineprefix()
{
    export WINEARCH=win32
    export WINEPREFIX=~/wineprefixes/win32
}

sub sx()
{
    startx &
    disown
    exit
}
