#!/bin/bash

set -euo pipefail

log() { echo "  action.sh: $*" >&2; }
die() { log "error: $*"; exit 1; }
run() { local c; c="$(printf ' %q' "$@")"; log "+$c"; "$@" || die "command failed ($?):$c"; }

action="$1"
file="$2"

file="$(readlink -f "$file")"

# log "running $action on $file ..."

markfile=/tmp/fehmark.txt

case "$action" in
    clipboard)
        path="$(printf "%q" "$file")"
        path="${path/#$HOME/~}"
        echo "$path" | run xclip -i -r
        ;;
    rename)
        run ~/bin/rofi-mv -t -c mv -e "$file"
        ;;
    convert)
        run ~/bin/rofi-mv -t -c convert "$file" && run rm -- "$file"
        ;;
    remove)
        run rm -- "$file"
        ;;
    mark)
        if grep -q -xFe "$file" "$markfile"
        then
            grep -v -xFe "$file" "$markfile" | sponge "$markfile"
            log "unmarked from $markfile: $file"
        else
            printf '%s\n' "$file" >> "$markfile"
            log "marked to $markfile: $file"
        fi
        ;;
    info)
        if [[ -f "$markfile" ]] && grep -q -xFe "$file" "$markfile"
        then
            echo "MARKED in $markfile"
        else
            echo ""
        fi
        ;;
    *)
        log "invalid action $action ..." >&2
        ;;
esac
