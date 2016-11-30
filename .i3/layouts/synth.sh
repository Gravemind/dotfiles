#!/bin/bash

HERE="$(cd "`dirname "$0"`"; pwd)"

WS="$1"
if [ -z "$WS" ]
then
	WS=4
fi

i3-msg "workspace $WS; append_layout $HERE/synth.json"

urxvt -name qjack -e zsh -is eval "qjackctl &" &
disown
urxvt -name qsynth -e zsh -is eval "sleep 1; qsynth &" &
disown

i3-msg "workspace 3"
