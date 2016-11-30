#!/bin/bash

HERE="$(cd "`dirname "$0"`"; pwd)"

$HERE/urxvt.sh 1 main
$HERE/browser.sh firefox 10

i3-msg workspace 1
i3-msg workspace 10
