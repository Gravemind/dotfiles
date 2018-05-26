#!/bin/zsh

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

_rgg() {
    s="$1"
    shift
    rg -uu --files --iglob '*'"$s"'*' "$@"
}
alias rgg='noglob _rgg'
compdef _rg rgg=rg

alias gdiff='pipes2files git d --no-index'
alias gdiffw='pipes2files git dw --no-index'

alias clean='find . \( -name "*~" -or -name "#*#" \) -print -delete'
alias fclean='find . \( -name "*~" -or -name "#*#" -or -name "*.o" -or -name "*.pyc" \) -print -delete'
alias cleansvn='find . -name ".svn" -execdir rm -rf {} \;'
alias cleandhcp='sudo rm -rf /var/lib/dhcp3/*.lease'
alias cleanhg='find . \( -name "*.orig" \) -print -delete'

# trailing whitespace will make it resolve aliases !
alias sudo='sudo -E '
alias _='sudo'

alias lock='i3lock -c 111111'
#alias disk='df -h --total'
alias x='dtrx -v'
alias xl='dtrx -l'
alias untar='tar xvzf'
alias a='echo -ne "\a"'

shut() {
    ( sleep 1 ; systemctl poweroff ) &
    disown
}
reboot() {
    ( sleep 1 ; systemctl reboot ) &
    disown
}

alias ssh-fingerprint="find . -name '*.pub' -exec ssh-keygen -l -f {} \;"

alias rchttpd='sudo /etc/rc.d/httpd'
alias rcmysqld='sudo /etc/rc.d/mysqld'
alias rcsshd='sudo /etc/rc.d/sshd'

alias -g    CAT='|& cat -A'
alias -g   GREP='|& grep -i'
alias -g   LESS='|& less'
alias -g  LESSS='|& less -S'
alias -g    ERR='2> /dev/null'
alias -g   NULL='>& /dev/null'
alias -g  COLOR='--color=always'
alias -g  XCILP='| xclip -r'
alias -g   YANK='| yank'

alias speedtest='wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test100.zip'
