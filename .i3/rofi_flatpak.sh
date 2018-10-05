#!/bin/bash

if [[ -z "$*" ]]
then
    flatpak list --app | cut -d/ -f1
else
    app="$1"
    logfile="/tmp/rofi_flatpak.$app.log"
    (
        date;
        flatpak run "$app";
        date;
    ) >& "$logfile" &
    disown
fi

