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

PS1='[\u@\h \W]\$ '
