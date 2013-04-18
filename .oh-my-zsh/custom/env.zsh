
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

export GREP_OPTIONS="--binary-files=without-match"

export EDITOR='emacs'
export PAGER='less'
export LESS='-R'
export PATH="$PATH:/home/jo/bin"
export OSTYPE=`uname -s`
export MACHTYPE=`uname -m`
export BROWSER='/usr/bin/firefox'

export WINEARCH="win32"
export WINEPREFIX="$HOME/win32"
