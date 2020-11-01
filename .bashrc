#!/usr/bin/env bash
#
# ~/.bashrc
#

export PATH="$HOME/bin:$PATH"
export PS1='\s-\v\$ ' # bash default prompt

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# emacs tramp
if [[ "$TERM" == "dumb" ]]
then
    export PS1='$ '
    return
fi

# if [ -f /usr/share/bash-completion/bash_completion ]
# then
#     . /usr/share/bash-completion/bash_completion
# elif [ -f /etc/bash_completion ]; then
#     . /etc/bash_completion
# fi

# append history to the HISTFILE
shopt -s histappend
# '**' matches zero or more directories and subdirectories
shopt -s globstar
# save all multiple-lines command to allow easy re-editing of multi-line commands
shopt -s cmdhist
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

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
export BROWSER='firefox'

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

# for systemd (journalctl...)
export SYSTEMD_LESS="$LESS -F"

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
alias md='mkdir -p'
alias rd='rmdir'
alias c='clear'
alias e='exit'
alias t='urxvt & ; disown'

alias cdr='cd "$(git rev-parse --show-toplevel)"'
alias gdiff='pipes2files git d --no-index'
alias gdiffw='pipes2files git dw --no-index'

alias lesss='less -+S'

alias topu='top -u "$(id -nu)"'

if ! command -v fd >& /dev/null && command -v fdfind >& /dev/null; then
    alias fd='fdfind'
fi

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias a='echo -ne "\a"'

take() {
    mkdir -p "$1" && cd "$1"
}

my_sourcing_again="${my_sourcing_again:-false}"
$my_sourcing_again || export _shell_depth="$((${_shell_depth:-0} + 1))"

my_prompt_command() {
    _PROMPT_STATUS=$?
    eval "${_MY_PREV_PROMPT_COMMAND:-}"
}
if ! $my_sourcing_again
then
    _MY_PREV_PROMPT_COMMAND="${PROMPT_COMMAND:-}"
    PROMPT_COMMAND=my_prompt_command
fi
export PS1='\[\033[1;30;40m\]\[\033[1;30m\]$_shell_depth \[\033[1;34m\]\u${SSH_CONNECTION:+\[\033[1;32m\]@\H} \[\033[1;34m\]\W\[\033[1;31m\]$([[ $_PROMPT_STATUS -eq 0 ]] || echo " $_PROMPT_STATUS") \[\033[1;37m\]\$\[\033[0;0;0m\] '
export ORIG_PS1="$PS1"

if false # true
then
    function _my_starship_precmd(){
        printf "\a" # alert
    }
    starship_precmd_user_func="_my_starship_precmd"
    export _STARSHIP_ENV_VAR=$(if ((_shell_depth > 1)); then echo $_shell_depth ; fi)
    eval "$(starship init bash)"
fi

my_sourcing_again=true
