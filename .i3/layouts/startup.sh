#!/bin/bash

HERE="$(cd "`dirname "$0"`"; pwd)"

$HERE/urxvt.zsh 1 main
$HERE/browser.zsh firefox 10

i3-msg workspace 1
i3-msg workspace 10
