#!/bin/bash

base0="$(basename "$0")"

fstats="/usr/lib/firetools/fstats"

if [[ -z "$*" ]]
then
    echo -en "\0markup-rows\x1ftrue\n" # enable markup

    echo "jails firestats $fstats"

    while IFS=: read -u3 -r pid user dd cmd
    do
        echo -n "Jail: $cmd ($user $pid)"
        echo -ne "\0icon\x1ffiretools\n"
    done 3< <( firejail --list )
else
    if [[ "${1}" =~ "$fstats"$ ]]
    then
        "$fstats" > /dev/null &
        disown
    fi
fi
