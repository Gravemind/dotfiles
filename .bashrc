#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# emacs tramp
if [[ "$TERM" == "dumb" ]]
then
    export PS1='$ '
    return
fi

# append history to the HISTFILE
shopt -s histappend
# '**' matches zero or more directories and subdirectories
shopt -s globstar
# save all multiple-lines command to allow easy re-editing of multi-line commands
shopt -s cmdhist

# add commands to the history file each time you hit enter
PROMPT_COMMAND="history -a"

# 'ignorespace' ignore commands that start with spaces
# 'ignoredups' ignore duplicates
# 'ignoreboth' both
HISTCONTROL=ignoreboth
# history size
HISTSIZE=10000
HISTFILESIZE=10000

export EDITOR='emacs'
export PAGER='less'

# `bash -x` "prompt" https://wiki.bash-hackers.org/scripting/debuggingtips
export PS4='+$$+${BASH_SOURCE}:${LINENO}+${FUNCNAME[0]:-}+ '

# less config
export LESS_TERMCAP_mb=$'\e[01;31m' # begin blinking (?)
export LESS_TERMCAP_md=$'\e[01;34m' # begin bold (titles)
export LESS_TERMCAP_me=$'\e[0m'     # end mode
export LESS_TERMCAP_so=$'\e[01;37m' # begin standout-mode (search)
#export LESS_TERMCAP_so=$'\E[7m'    # begin standout-mode (search)
export LESS_TERMCAP_se=$'\e[0m'     # end standout-mode
export LESS_TERMCAP_us=$'\e[01;36m' # begin underline (words)
export LESS_TERMCAP_ue=$'\e[0m'     # end underline
export LESSBINFMT='*u<%02X>'
export LESS='-qRiSM -j5 -z-4'
export MANPAGER='less -J'

# enable sysstat (sar) colors
export S_COLORS=true

# ripgrep rg
export RIPGREP_CONFIG_PATH=$HOME/.config/ripgrep/config

alias grep='grep --color=auto'
alias less='less --quiet'
alias cat='cat -v'

alias ls='ls -h --color=auto'
alias l='ls -l'
alias la='ls -la'
alias lt='l -rt'

alias rcp='cp -r'
alias c='clear'
alias e='exit'
alias t='urxvt & ; disown'

alias lesss='less -+S'

alias topu='top -u "$(id -nu)"'

take() {
    mkdir -p "$1" && cd "$1"
}

PS1='[\u@\h \W]\$ '
