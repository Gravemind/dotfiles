#!/usr/bin/env bash

get_curr_id() {
	HEX=`xprop -root _NET_ACTIVE_WINDOW | sed -r 's/.*0x([0-9a-f]+)$/\1/g'`
	CURR=`echo "obase=10; ibase=16; $HEX" | bc`
	echo $CURR
}

CURR_WID=`get_curr_id`

i3-msg '[urgent="oldest"] focus'

sleep 0.1

NEW_WID=`get_curr_id`

OUTF="$HOME/.i3/last_focus"

if [ "$CURR_WID" = "$NEW_WID" ]
then
	if [ -f "$OUTF" ]
	then
		ID=`cat $OUTF`
		rm $OUTF
		i3-msg "[id=\"$ID\"] focus"
	fi
else
	echo -n "$CURR_WID" > "$OUTF"
fi
