#!/bin/bash

base0="$(basename "$0")"

if [[ -z "$*" ]]
then
    echo -en "\0markup-rows\x1ftrue\n" # enable markup
    while read -u3 -r app description
    do
        name="${description%% -*}"
        desc="${description#*- }"
        echo "$name"' <span size="small" alpha="80%">'"$app"'</span>'
    done 3< <( flatpak list --app --columns=app,description )
else
    app="$*"
    app="${app%</*}"
    app="${app##*>}"
    logfile="/tmp/$base0.$USER.$app.log"
    umask 077
    (
        date;
        flatpak run "$app";
        date;
    ) >& "$logfile" &
    disown
fi

