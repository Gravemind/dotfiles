
# export LESS_TERMCAP_mb=$'\E[01;34m'       # begin blinking
# export LESS_TERMCAP_md=$'\E[01;34m'       # begin bold
# export LESS_TERMCAP_me=$'\E[0m'           # end mode
# export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
# export LESS_TERMCAP_so=$'\E[0m\E[1m'      # begin standout-mode - info box
# export LESS_TERMCAP_ue=$'\E[0m'           # end underline
# export LESS_TERMCAP_us=$'\E[01;36m'       # begin underline

export LESS_TERMCAP_mb=$'\E[01;34m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;34m'       # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[1;37m'      # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[01;36m'       # begin underline

export LESSBINFMT='*u<%02X>'

# -F : --quit-if-one-screen
# -R : --RAW-CONTROL-CHARS: allow ANSI "color" escape sequences
# -X : --no-init : no termcap clear on quit
# -i : smart ignore case
# -I : always ignore case
# -S : --chop-long-lines : truncate lines
# -M : --LONG-PROMPT
# -jn : --jump-target=n
export LESS='-R -i -S -M -j5'

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
