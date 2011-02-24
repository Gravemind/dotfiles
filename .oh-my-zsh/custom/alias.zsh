alias grep='grep --color=always'
alias less='less --quiet'
alias cat='cat -v'

alias l='ls -l'
alias la='ls -la'
alias lt='l -rt'

alias c='clear'
alias e='exit'
alias j='jobs'

alias clean='find . \( -name "*~" -or -name "#*#" \) -print -delete'
alias fclean='find . \( -name "*~" -or -name "#*#" -or -name "*.o" -or -name "*.pyc" \) -print -delete'
alias cleansvn='find . -name ".svn" -execdir rm -rf {} \;'
alias cleandhcp='sudo rm -rf /var/lib/dhcp3/*.lease'

alias lock='i3lock -c 111111'
alias shut='sudo shutdown -h now'
alias reboot='sudo reboot'
alias disk='df -h --total'
alias rchttpd='sudo /etc/rc.d/httpd'
alias rcmysqld='sudo /etc/rc.d/mysqld'
alias rcsshd='sudo /etc/rc.d/sshd'
