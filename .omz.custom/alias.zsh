#!/bin/zsh

alias grep='grep --binary-files=without-match --color=auto'
alias cat='cat -v'
# for less, see ./less.zsh

alias ls='ls -h --color=auto'
alias l='ls -l'
alias la='ls -la'
alias lt='l -rt'

alias rcp='cp -r'
alias c='clear'
alias e='exit'
alias t='urxvt & ; disown'

_rgg() {
	s="$1"
	shift
	rg -uu --files --iglob '*'"$s"'*' "$@"
}
alias rgg='noglob _rgg'
compdef _rg rgg=rg

alias gdiff='pipes2files git d --no-index'
alias gdiffw='pipes2files git dw --no-index'

alias clean='find . \( -name "*~" -or -name "#*#" -or -name ".#*" \) -print -delete'
alias fclean='find . \( -name "*~" -or -name "#*#" -or -name ".#*" -or -name "*.o" -or -name "*.pyc" \) -print -delete'
alias cleansvn='find . -name ".svn" -execdir rm -rf {} \;'
alias cleandhcp='sudo rm -rf /var/lib/dhcp3/*.lease'
alias cleanhg='find . \( -name "*.orig" \) -print -delete'

# trailing whitespace will make it resolve aliases !
alias sudo='sudo -E '
alias _='sudo'

#alias disk='df -h --total'
alias untar='tar xvzf'
alias a='echo -ne "\a"'

alias pydb='python -m pdb '

shut() {
	( sleep 1 ; systemctl poweroff ) &
	disown
}
reboot() {
	( sleep 1 ; systemctl reboot ) &
	disown
}

alias suspend='systemctl suspend'

alias ssh-fingerprint="find . -name '*.pub' -exec ssh-keygen -l -f {} \;"

alias rchttpd='sudo /etc/rc.d/httpd'
alias rcmysqld='sudo /etc/rc.d/mysqld'
alias rcsshd='sudo /etc/rc.d/sshd'

alias -g    CAT='|& cat -A'
alias -g   GREP='|& grep -i'
alias -g    TEE='|& tee'
alias -g    ERR='2> /dev/null'
alias -g   NULL='>& /dev/null'
alias -g  COLOR='--color=always'
alias -g  XCLIP='| tee >(xclip -r)'
alias -g   YANK='| yank'
alias -g     BG='& ; disown'

alias speedtest='wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test100.zip'
