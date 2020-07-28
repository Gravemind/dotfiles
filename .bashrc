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
alias c='clear'
alias e='exit'
alias t='urxvt & ; disown'

alias lesss='less -+S'

alias topu='top -u "$(id -nu)"'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

take() {
    mkdir -p "$1" && cd "$1"
}

export PS1='\[\033[1;30;40m\]\[\033[1;30m\]$([[ $SHLVL -eq 1 ]] || echo "$SHLVL ")\[\033[1;34m\]\u${SSH_CONNECTION:+\[\033[1;32m\]@\H} \[\033[1;34m\]\W\[\033[1;31m\]$(r=$?; [[ $r -eq 0 ]] || echo " $r") \[\033[1;37m\]\$\[\033[0;0;0m\] '
