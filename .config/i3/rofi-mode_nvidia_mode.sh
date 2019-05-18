#!/bin/bash

base0="$(basename "$0")"

main() {
    local i="$1"

    RERUN=true
    while $RERUN
    do
        RERUN=false
        nv_update_pties
        echo -en "\0active\x1f4\n"
        true &&
            entry_mode "$i" " Game:   VSync + VRR + Indicators" 1 1 1 &&
            entry_mode "$i" " VRR:    VSync + VRR" 1 1 0 &&
            entry_mode "$i" " VSync:  VSync" 1 0 0 &&
            entry_mode "$i" " Unsync: -" 0 0 0 &&
            #entry_comm "$i" "--" &&
            entry_gsync "$i" &&
            entry_prop "$i" AllowVRR &&
            entry_prop "$i" AllowFlipping &&
            entry_prop "$i" SyncToVBlank &&
            entry_prop "$i" ShowGraphicsVisualIndicator &&
            entry_prop "$i" ShowVRRVisualIndicator
        i=""
    done
}

entry_mode() {
    local input="$1"
    local entry="$2"
    local vsync="$3"
    local vrr="$4"
    local ind="$5"
    if [[ -z "$input" ]]; then
       echo "$entry"
    elif [[ "$input" = "$entry" ]]; then

        # Keep GSyncCompatible
        if [[ $vrr = 1 ]]; then
           nv_gsync_comp $vrr
        fi
        nv_set AllowVRR $vrr

        nv_set AllowFlipping $vsync
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
    local v="$(nv_get "$pty")"
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
             nv_set "$pty" "0"
         else
             nv_set "$pty" "1"
         fi
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

# way faster to query all at once than one by one
ALL_NV_ATTRIBS=""
nv_update_pties() {
    ALL_NV_ATTRIBS="$(nvidia-settings -q all | perl -ne 's/^  Attribute '\''(.*?)'\'' .*?\): (.*?)\.?$/ print $1." ".$2."\n"; /e')"
}

nv_gsync_comp() {
    local enable="${1:-}"
    local mode="$(nv_get CurrentMetaMode)"
    mode="${mode#* :: }"
    if [[ -z "$enable" ]]; then
        if [[ "$mode" =~ "AllowGSYNCCompatible=On" ]]; then
            echo 1
        else
            echo 0
        fi
    else
        if [[ "$enable" = 1 ]]; then
            if [[ "$mode" =~ "AllowGSYNCCompatible=Off" ]]; then
                nv_set CurrentMetaMode "${mode/AllowGSYNCCompatible=Off/AllowGSYNCCompatible=On}"
            elif [[ ! "$mode" =~ "AllowGSYNCCompatible=On" ]]; then
                nv_set CurrentMetaMode "${mode/ \{ViewPortIn=/ \{AllowGSYNCCompatible=On, ViewPortIn=}"
            fi
        else
            if [[ "$mode" =~ "AllowGSYNCCompatible=On" ]]; then
                nv_set CurrentMetaMode "${mode/AllowGSYNCCompatible=On/AllowGSYNCCompatible=Off}"
            fi
        fi
    fi
}

nv_get() {
    local v="$(echo "$ALL_NV_ATTRIBS" | grep "^$1 ")"
    v="${v#* }"
    echo "$v"
}

nv_set() {
    nvidia-settings -a "$1=$2" 2>&1 1> /dev/null &&
        echo "$base0: successfully set $1=$2" >&2 ||
        echo "$base0: error: failed to set $1=$2" # >&2
}

main "$@"
