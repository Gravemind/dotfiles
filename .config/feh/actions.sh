#!/bin/bash

action="$1"
file="$2"

echo "running $action on $file ..." >&2

case "$action" in
    clipboard)
        path="$(printf "%q" "$(readlink -f "$file")")"
        path="${path/#$HOME/~}"
        echo "$path" | xclip -i -r
        ;;
    rename)
        ~/bin/rofi-mv -t -c mv -e "$file"
        ;;
    convert)
        ~/bin/rofi-mv -t -c convert "$file" && rm "$file"
        ;;
    remove)
        rm "$file"
        ;;
    *)
        echo "invalid action $action ..." >&2
        ;;
esac
