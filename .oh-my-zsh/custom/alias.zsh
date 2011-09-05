#!/bin/zsh

alias grep='grep --color=auto'
alias less='less --quiet'
alias cat='cat -v'

alias l='ls -l'
alias la='ls -la'
alias lt='l -rt'

alias rcp='cp -r'
alias c='clear'
alias e='exit'

alias clean='find . \( -name "*~" -or -name "#*#" \) -print -delete'
alias fclean='find . \( -name "*~" -or -name "#*#" -or -name "*.o" -or -name "*.pyc" \) -print -delete'
alias cleansvn='find . -name ".svn" -execdir rm -rf {} \;'
alias cleandhcp='sudo rm -rf /var/lib/dhcp3/*.lease'

alias _='command sudo -E ' # ! trailing wihtespace is important
alias lock='i3lock -c 111111'
alias shut='sudo shutdown -h now'
alias reboot='sudo reboot'
alias disk='df -h --total'
alias x='7z x'
alias flux='xflux -l 49 -k 4000'
alias untar='tar xvzf'
alias ntpup='sudo ntpd -qg'

if [[ -n "$PACMAN" ]]
then
    alias pacman="$PACMAN"
    compdef _pacman $PACMAN=pacman
fi

alias emacs='ne -q -l ~/.emacs.d/emacs-ide.el'
alias nw='\emacs -nw'

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
alias -g    COL='--color=always'
