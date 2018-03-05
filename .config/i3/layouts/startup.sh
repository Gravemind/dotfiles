#!/bin/bash

here="${0%/*}"

$here/launch.sh -w 1 -- urxvt &
disown
#$here/launch.sh -w 10 -- firefox &
#disown

i3-msg workspace 1
#i3-msg workspace 10
