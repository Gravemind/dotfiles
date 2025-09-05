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

screensh="$HOME/Pictures/$(date '+screen_%y-%m-%d_%H:%M:%S.png')"

if [[ $select = 1 ]]
then
    grim -g "$(slurp -d)" "$screensh"
else
    grim "$screensh"
fi
echo "$screensh"

if [[ $edit = 1 ]]
then
    swappy -f "$screensh"
fi
