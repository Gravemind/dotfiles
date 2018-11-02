#!/usr/bin/env bash

nextwid_file="/tmp/goto_urgent_and_back.$USER"

get_curr_id() {
	local hex="$(xprop -root _NET_ACTIVE_WINDOW | sed -r 's/.*0x([0-9a-f]+)$/\1/g')"
	printf "%d\n" "0x$hex"
}

old_wid="$(get_curr_id)"
echo "$0: current window is $old_wid"

next_wid="$(cat $nextwid_file 2> /dev/null)"
echo -n "$old_wid" > $nextwid_file

res="$(i3-msg '[urgent="oldest"] focus' 2> /dev/null)"
if [[ "$res" == *"\"success\":true"* ]]
then
	echo "$0: focused urgent window"
else
	if [[ -n "$next_wid" ]]
	then
		echo "$0: focusing window $next_wid"
		i3-msg "[id=\"$next_wid\"] focus" > /dev/null
	fi
fi
