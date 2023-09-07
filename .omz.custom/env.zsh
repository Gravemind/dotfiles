#!/bin/zsh

if [[ -z "$ORIG_PATH" ]]
then
    export ORIG_PATH="$PATH"
    export ORIG_LD_LIBRARY_PATH="$LD_LIBRARY_PATH"
fi

# ripgrep rg
export RIPGREP_CONFIG_PATH=$HOME/.config/ripgrep/config

# CMake always generate a compile_commands.json
export CMAKE_EXPORT_COMPILE_COMMANDS=1

# deprecated:
#export GREP_OPTIONS="--binary-files=without-match"
export GREP_OPTIONS=

# enable sysstat (sar) colors
export S_COLORS=true

export EDITOR='emacs'
export PAGER='less'

if type firefox >& /dev/null
then
    export BROWSER="firefox"
fi

export OSTYPE=`uname -s`
export MACHTYPE=`uname -m`
export OS=`uname -s`

# add bell (urgent alert) to sudo password prompt
export SUDO_PROMPT=$'\a'"[sudo] [%u@%h] password for %p: "

# `bash -x` "prompt" https://wiki.bash-hackers.org/scripting/debuggingtips
export PS4='+$$+${BASH_SOURCE:-?}:${LINENO:-?}+${FUNCNAME[0]:-?}+ '
