#!/usr/bin/bash

clones() {
    local tmp
    tmp="$(mktemp -t omz-clones.XXXXXX)"

    command clones --parent-sh-do="$tmp" "$@" || {
        local r=$?
        rm -f "$tmp"
        return $r
    }

    local actions
    actions=$(<"$tmp")
    rm -f "$tmp" ||:

    while read -r action
    do
        case "$action" in
            '') ;;
            "cd "*)
                local d="${action#cd }"
                echo cd "$d"
                cd "$d"
                ;;
            *)
                echo "clones: error: unknown parent-sh-do: $action"
                ;;
        esac
    done <<<"$actions"
}
