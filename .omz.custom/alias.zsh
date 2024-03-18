#!/bin/zsh

alias grep='grep --binary-files=without-match --color=auto'
alias cat='cat -v'
# for less, see ./less.zsh

alias l='ls -h --color=auto -l'
alias la='l -a'
alias lt='l -rt'

# alias l='exa -l'
# alias la='exa -la'
# alias lt='exa -l --sort=newest'

alias rcp='cp -r'
alias md='mkdir -p'
alias rd='rmdir'
alias c='clear'
alias e='exit'
alias t='urxvt & ; disown'

# Safe xargs
alias xargss='xargs -d \\n -r'

_rgg() {
	s="$1"
	shift
	rg -uu --files --iglob '*'"$s"'*' "$@"
}
alias rgg='noglob _rgg'
compdef _rg rgg=rg

if ! command -v fd >& /dev/null && command -v fdfind >& /dev/null; then
	alias fd='fdfind'
fi

gdiff() { pipes2files git d --no-index "$@"; }
gdiffw() { pipes2files git dw --no-index "$@"; }
run-help-gdiff() { echo "run-help gdiff $*"; run-help git-diff; }
run-help-gdiffw() { echo "run-help gdiffw $*"; run-help git-diff; }

alias clean='find . \( -name "*~" -or -name "#*#" -or -name ".#*" \) -print -delete'
alias fclean='find . \( -name "*~" -or -name "#*#" -or -name ".#*" -or -name "*.o" -or -name "*.pyc" \) -print -delete'
alias cleansvn='find . -name ".svn" -execdir rm -rf {} \;'
alias cleandhcp='sudo rm -rf /var/lib/dhcp3/*.lease'
alias cleanhg='find . \( -name "*.orig" \) -print -delete'

# trailing whitespace will make it resolve aliases !
# alias sudo='sudo -E '
alias sudo='sudo '
alias _='sudo -E '

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
#alias -g  XCLIP='| tee >(xclip -r)'
alias -g  XCLIP='| tee >(rmlastnl | clipcopy)' # .oh-my-zsh/lib/clipboard.zsh
alias -g   YANK='| yank'
alias -g     BG='& ; disown'
alias -g  ERROR="2> >(sed -u 's/^/'\"$fg_bold[red]\"'stderr:'\"$reset_color\"' /g' >&2)"
alias -g     JQ='| jq . -C | less'

alias speedtest='wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test100.zip'

# usage: BASH <anything; realy *, passed literally to bash -c>
#   Runs everything after BASH in bash -c '...'
#   https://unix.stackexchange.com/questions/456508/zsh-print-literal-text-from-the-command-line
bash_literally () {
  local str="$history[$(print -P %h)]"
  bash -c "${str#*BASH\ }"
}
alias BASH='bash_literally #'
# zmodload zsh/parameter
# setopt interactivecomments

rmlastnl() {
	perl -pe 'chomp if eof'
}
