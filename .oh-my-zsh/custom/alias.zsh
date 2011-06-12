alias grep='grep --color=always'
alias less='less --quiet'
alias cat='cat -v'

alias l='ls -l'
alias la='ls -la'
alias lt='l -rt'

alias c='clear'
alias e='exit'

alias clean='find . \( -name "*~" -or -name "#*#" \) -print -delete'
alias fclean='find . \( -name "*~" -or -name "#*#" -or -name "*.o" -or -name "*.pyc" \) -print -delete'
alias cleansvn='find . -name ".svn" -execdir rm -rf {} \;'
alias cleandhcp='sudo rm -rf /var/lib/dhcp3/*.lease'

alias lock='i3lock -c 111111'
alias shut='sudo shutdown -h now'
alias reboot='sudo reboot'
alias disk='df -h --total'
alias x='7z x'
alias flux='xflux -l 49 -k 4000'
alias untar='tar xvzf'

alias emacs='ne -q -l ~/.emacs.d/emacs-ide.el'
alias nw='\emacs -nw'

alias rchttpd='sudo /etc/rc.d/httpd'
alias rcmysqld='sudo /etc/rc.d/mysqld'
alias rcsshd='sudo /etc/rc.d/sshd'

alias -g CAT='|& cat -A'
alias -g DN='/dev/null'
alias -g GREP='|& grep'
alias -g LESS='|& less'
alias -g MORE='|& more'
alias -g TAIL='|& tail'
alias -g ERR='2> /dev/null'
