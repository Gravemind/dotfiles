#!/bin/zsh

export RIPGREP_CONFIG_PATH=$HOME/.config/ripgrep/config

# deprecated:
#export GREP_OPTIONS="--binary-files=without-match"
export GREP_OPTIONS=

## enable sysstat (sar) colors
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

export HISTSIZE=100000
export SAVEHIST=100000

export ORIG_PATH="$PATH"
