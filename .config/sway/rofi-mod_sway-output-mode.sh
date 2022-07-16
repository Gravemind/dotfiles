#!/bin/bash

base0="$(basename "$0")"

main() {
    local i="$1"

    RERUN=true
    while $RERUN
    do
        RERUN=false
        # nv_update_pties
        update_pties
        echo -en "\0active\x1f4\n"
        true &&
            # entry_mode "$i" " Sync:        VSync" 1 0 0 &&
            # entry_mode "$i" " Game VRR:    VSync + VRR + Indicators" 1 1 1 &&
            # entry_mode "$i" " Game Sync:   VSync + Indicators" 1 0 1 &&
            # entry_mode "$i" " Game Unsync: Unsync + Indicators" 0 0 1 &&
            #entry_comm "$i" "--" &&
            entry_prop "$i" adaptive_sync &&
            entry_prop_int "$i" max_render_time &&
            # entry_prop "$i" SyncToVBlank &&
            # entry_prop "$i" AllowVRR &&
            # entry_gsync "$i" &&
            # entry_prop "$i" ShowGraphicsVisualIndicator &&
            # entry_prop "$i" ShowVRRVisualIndicator &&
            true
        i=""
    done
}

entry_mode() {
    local input="$1"
    local entry="$2"

    local vsync="$3"
    local vrr="$4"
    local ind="$5"

    local flip="1" # always allow flipping !?

    if [[ -z "$input" ]]; then
       echo "$entry"
    elif [[ "$input" = "$entry" ]]; then

        # Keep GSyncCompatible
        if [[ $vrr = 1 ]]; then
           nv_gsync_comp $vrr
        fi
        nv_set AllowVRR $vrr

        nv_set AllowFlipping $flip
        nv_set SyncToVBlank $vsync

        nv_set ShowGraphicsVisualIndicator $ind
        nv_set ShowVRRVisualIndicator $ind

        return 1
    fi
}

entry_gsync() {
    local input="$1"
    local entry=""
    local v="$(nv_gsync_comp)"
    if [[ "$v" = 1 ]]; then
        entry=" GSyncCompatible"
    else
        entry=" GSyncCompatible"
    fi
    if [[ -z "$input" ]]; then
        echo "$entry"
    elif [[ "$input" = "$entry" ]]; then
        if [[ "$v" = 1 ]]; then
           nv_gsync_comp 0
        else
           nv_gsync_comp 1
        fi
        RERUN=true
        return 1
    fi
}

entry_prop() {
    local input="$1"
    local pty="$2"
    local v="$(sway_output_get "$pty")"
    local entry=""
    if [[ "$v" = 1 ]]; then
        #entry="   $pty"
        entry=" $pty"
    else
        entry=" $pty"
    fi
    if [[ -z "$input" ]]; then
        echo "$entry"
    elif [[ "$input" = "$entry" ]]; then
        if [[ "$v" = "1" ]]
        then
            sway_output_set "$pty" "0"
        else
            sway_output_set "$pty" "1"
        fi
        RERUN=true
        return 1 # break
    fi
}

entry_prop_int() {
    local input="$1"
    local pty="$2"

    local v="$(sway_output_get "$pty")"
    local entry=""
    #entry="   $pty"
    entry="$v $pty"
    if [[ -z "$input" ]]; then
        echo "+ $entry"
        echo "- $entry"
    elif [[ "$input" = "+ $entry" ]]; then
        if [[ "$v" = off || "$v" -le 0 ]]; then
            v=1
        else
            v=$((v + 1))
        fi
        sway_output_set "$pty" "$v"
        RERUN=true
        return 1 # break
    elif [[ "$input" = "- $entry" ]]; then
        if [[ "$v" = off || "$v" -le 1 ]]; then
            v=off
        else
            v=$((v - 1))
        fi
        sway_output_set "$pty" "$v"
        RERUN=true
        return 1 # break
    fi
}

entry_comm() {
    local input="$1"
    shift
    if [[ -z "$input" ]]; then
        echo "$@"
    fi
}

ALL_SWAY_OUTPUT=""
update_pties() {
    ALL_SWAY_OUTPUT="$(swaymsg -t get_outputs --raw)"
}

sway_output_get() {
    local pty="$1"

    # Remap to get_outputs pties
    case "$pty" in
    adaptive_sync) pty=adaptive_sync_status; ;;
    esac

    local val="$(jq -r --arg pty "$pty" '.[0][$pty]' <<<"$ALL_SWAY_OUTPUT")"
    case "$val" in
    on|enabled)
        echo 1
        ;;
    off|disabled)
        echo 0
        ;;
    *)
        echo "$val"
        ;;
    esac
    echo "$pty=$val" >&2
}

sway_output_set() {
    local pty="$1"
    local val="$2"

    local res
    res="$(swaymsg output "*" "$pty" "$val")"

    local check_restart=0
    case "$pty" in
    adaptive_sync) check_restart=1; ;;
    esac

    if [[ $check_restart = 1 ]]; then
        sleep 0.1s
        update_pties
        if [[ "$(sway_output_get "$pty")" != "$val" ]]; then
            # FIXME multi screen monitor
            output="$(sway_output_get name)"
            res="$(swaymsg output "$output" disable ||:)"
            sleep 0.2s
            res="$(swaymsg output "$output" enable ||:)"
        fi
    fi
}

main "$@"
