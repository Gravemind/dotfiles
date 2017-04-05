#!/bin/bash

here="${0%/*}"

$here/launch.sh -w 1 -- urxvt
$here/launch.sh -w 10 -- firefox

i3-msg workspace 1
i3-msg workspace 10
