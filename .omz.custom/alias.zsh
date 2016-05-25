#!/bin/zsh

alias grep='grep --color=auto'
alias less='less --quiet'
alias cat='cat -v'

alias ls='ls -h --color'
alias l='ls -l'
alias la='ls -la'
alias lt='l -rt'

alias rcp='cp -r'
alias c='clear'
alias e='exit'

alias clean='find . \( -name "*~" -or -name "#*#" \) -print -delete'
alias fclean='find . \( -name "*~" -or -name "#*#" -or -name "*.o" -or -name "*.pyc" \) -print -delete'
alias cleansvn='find . -name ".svn" -execdir rm -rf {} \;'
alias cleansvn='find . -name ".svn" -execdir rm -rf {} \;'
alias cleandhcp='sudo rm -rf /var/lib/dhcp3/*.lease'
alias cleanhg='find . \( -name "*.orig" \) -print -delete'

# trailing whitespace will make it resolve aliases !
alias _='command sudo -E '
alias sudo='command sudo -E '

alias lock='i3lock -c 111111'
alias disk='df -h --total'
alias x='dtrx -v'
alias xl='dtrx -l'
alias flux='xflux -l 49 -k 4000'
alias untar='tar xvzf'
alias a='echo -ne "\a"'

alias shut='systemctl poweroff'
alias reboot='systemctl reboot'

alias start='_ rc.d start'
alias stop='_ rc.d stop'
alias restart='_ rc.d restart'

alias ssh-fingerprint="find . -name '*.pub' -exec ssh-keygen -l -f {} \;"

alias rchttpd='sudo /etc/rc.d/httpd'
alias rcmysqld='sudo /etc/rc.d/mysqld'
alias rcsshd='sudo /etc/rc.d/sshd'

alias -g    CAT='|& cat -A'
alias -g   GREP='|& grep'
alias -g  GREPI='|& grep -i'
alias -g  GREPR='|& grep -rni'
alias -g   LESS='|& less'
alias -g  LESSS='|& less -S'
alias -g   MORE='|& more'
alias -g   TAIL='|& tail'
alias -g     DN='/dev/null'
alias -g    ERR='2> /dev/null'
alias -g    NULL='>& /dev/null'
alias -g    COL='--color=always'
alias -g	LINE='| wc -l'

alias speedtest='wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test100.zip'
