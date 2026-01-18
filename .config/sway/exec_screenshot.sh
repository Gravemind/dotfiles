#!/bin/bash

set -euo pipefail

select=0
edit=0
while [[ $# -gt 0 ]]
do
    case "$1"
    in
        --select) select=1 ;;
        --edit) edit=1 ;;
        *)
            echo "$0: invalid arg: $1" >&2;
            exit 1
            ;;
    esac
    shift
done

screendir="$HOME/Pictures"

screensh="$screendir/$(date '+screen_%Y-%m-%d_%H:%M:%S.png')"
if [[ -e "$screensh" ]]
then
    screensh="$screendir/$(date '+screen_%Y-%m-%d_%H:%M:%S.%N.png')"
fi
touch "$screensh"

if [[ $select = 1 ]]
then
    grim -g "$(slurp -d)" "$screensh"
else
    grim "$screensh"
fi
echo "$screensh"

notify-send -u low "${screensh/#$HOME/'~'}"

if [[ $edit = 1 ]]
then
    swappy -f "$screensh"
fi
