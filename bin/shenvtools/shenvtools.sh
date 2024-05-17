#!/bin/bash
# OR zsh

# envappend <name> <sep> <value>
#   Add <value> to the <sep>-seperated variable named <name>
envappend() {
    local var="$1"
    local sep="$2"
    local value="$3"

    _shenvtools_silent envremove "$var" "$sep" "$value"

    local oldv="$(_shenvtools_get "$var")"
    local v="$oldv"
    if [[ -z "$v" ]]; then
        v="$value"
    else
        v="$v$sep$value"
    fi
    if [[ "$v" != "$oldv" ]]; then
        _shenvtools_export "$var" "$v"
        _shenvtools_log "$var += $value"
    else
        _shenvtools_log "($var already have $value)"
    fi
}

# envprepend <name> <sep> <value>
#   Prepend <value> to the <sep>-seperated variable named <name>
envprepend() {
    local var="$1"
    local sep="$2"
    local value="$3"

    _shenvtools_silent envremove "$var" "$sep" "$value"

    local oldv="$(_shenvtools_get "$var")"
    local v="$oldv"
    if [[ -z "$v" ]]; then
        v="$value"
    else
        v="$value$sep$v"
    fi
    if [[ "$v" != "$oldv" ]]; then
        _shenvtools_export "$var" "$v"
        _shenvtools_log "$var += $value"
    else
        _shenvtools_log "($var already += $value)"
    fi
}

# envremove <name> <sep> <value>
#   Remove <value> from the <sep>-seperated variable named <name>
envremove() {
    local var="$1"
    local sep="$2"
    local value="$3"

    local oldv="$(_shenvtools_get "$var")"
    local v="$oldv"
    v="$sep$v$sep"
    v="${v//$sep$value$sep/$sep}"
    while [[ "$v" == "$sep"* ]] ; do
        v="${v#$sep}"
    done
    while [[ "$v" == *"$sep" ]] ; do
        v="${v%$sep}"
    done
    if [[ "$v" != "$oldv" ]]; then
        _shenvtools_export "$var" "$v"
        _shenvtools_log "$var -= $value"
    else
        _shenvtools_log "($var already -= $value)"
    fi
}

# addpath <path>
#   Add <path> to PATH
addpath() {
    local p="$1"
    _shenvtools_test "invalid path" -d "$p" || true
    _shenvtools_makeabs p
    envprepend "PATH" : "$p"
}

# rmpath <path>
#   Remove <path> from PATH
rmpath() {
    local p="$1"
    _shenvtools_makeabs p
    envremove "PATH" : "$p"
}

# addpath alias
addbin() { adpath "$@"; }

# rmpath alias
rmbin() { rmpath "$@"; }

# addlib <path>
#   Add <path> to LD_LIBRARY_PATH
addlib() {
    local p="$1"
    _shenvtools_test "invalid path" -d "$p" || true
    _shenvtools_makeabs p
    envprepend "LD_LIBRARY_PATH" : "$p"
}

# rmlib <path>
#   Remove <path> from LD_LIBRARY_PATH
rmlib() {
    local p="$1"
    _shenvtools_makeabs p
    envremove "LD_LIBRARY_PATH" : "$p"
}

# addtool <prefix_path>
#   Adds a tool installed in <prefix_path> to PATH, LD_LIBRARY_PATH, MANPATH, etc...
addtool() {
    local p="$1"
    _shenvtools_makeabs p

    addpath "$p/bin"

    local didlib=0
    [[ ! -d "$p/lib" ]] || { addlib "$p/lib"; didlib=1; }
    [[ ! -d "$p/lib64" ]] || { addlib "$p/lib64"; didlib=1; }
    _shenvtools_test "no lib/lib64 dir" "$didlib" -eq 1 || true

    for man in "$p/man" "$p/share/man"; do
        [[ ! -d "$man" ]] || envprepend "MANPATH" : "$man"
    done
}

# rmtool <prefix_path>
#   Remove a tool added by addtool
rmtool() {
    local p="$1"
    _shenvtools_makeabs p

    rmpath "$p/bin"
    rmlib "$p/lib"
    rmlib "$p/lib64"
    for man in "$p/man" "$p/share/man"; do
        envremove "MANPATH" : "$man"
    done
}

_shenvtools_makeabs() {
    local var="$1"

    local v="$(_shenvtools_get "$var")"
    v="$(realpath -mL "$v")"
    _shenvtools_set "$var" "$v"
}

_shenvtools_test() {
    local msg="$1"
    shift
    if ! test "$@" ; then
        echo "shenvtools: test failed: $msg ($*)" >&2
        return 1
    fi
    return 0
}

# Indirect get variable
_shenvtools_get() {
    local var="$1"
    if [ -n "${ZSH_VERSION:-}" ]; then
        echo -n "${(P)var}"
    elif [ -n "${BASH_VERSION:-}" ]; then
        : "${!var:=}" # support set -u
        echo -n "${!var}"
    fi
}

# Indirect set variable
_shenvtools_set() {
    local var="$1"
    local value="$2"
    read -r "$var" <<<"$value"

    # _shenvtools_test "$(_shenvtools_get)" "==" "value" >&2 || true
}

# Indirect export variable
_shenvtools_export() {
    local var="$1"
    local value="$2"
    export "$var=$value"
}

_shenvtools_log() {
    [[ "${SHENVTOOLS_VERBOSE:-0}" = 1 ]] || return 0
    echo "shenvtools: $*" >&2
}

_shenvtools_silent() {
    local verb="${SHENVTOOLS_VERBOSE:-}"
    SHENVTOOLS_VERBOSE=0
    "$@"
    SHENVTOOLS_VERBOSE="$verb"
}

SHENVTOOLS_VERBOSE=1
