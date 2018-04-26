#!/bin/zsh

## http://zsh.sourceforge.net/Doc/Release/Options.html

unsetopt share_history
unsetopt correctall
#unsetopt extended_glob

## unset option that nices background jobs
unsetopt bg_nice

## wait 10 seconds for 'rm *'.
## (force stop wait with Ctrl-C, or expand the '*' with TAB)
setopt rm_star_wait
