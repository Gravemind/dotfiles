
# see .oh-my-zsh/plugins/systemd/systemd.plugin.zsh for sudo or not sudo

__systemctl_journal() {
	local sudo="${1:-}"
	shift
	$sudo echo "$fg_bold[blue]Begin $sudo systemctl $@ & ...$reset_color"
	begin="$(date +'%F %T')"
	{
		$sudo systemctl --no-pager "$@"
		r="$?"
		if [[ "$r" -ne 0 ]] ; then
			echo "$fg_bold[blue]... End, $sudo systemctl $@ & $fg_bold[red]failed ($r)$fg_bold[blue] (Ctrl-C to quit)$reset_color"
		else
			echo "$fg_bold[blue]... End, $sudo systemctl $@ & $fg_bold[green]succeeded$fg_bold[blue] (Ctrl-C to quit)$reset_color"
		fi
	} &
	journalctl -f --since "$begin"
}
_systemctl_journal() {
	__systemctl_journal "" "$@"
}
compdef _systemctl_journal='systemctl'
_sudo_systemctl_journal() {
	__systemctl_journal "sudo" "$@"
}
compdef _sudo_systemctl_journal='systemctl'

alias sdr='sudo systemctl daemon-reload'
alias sdru='systemctl --user daemon-reload'

alias sd='sudo systemctl'
alias sdu='systemctl --user'

alias sdj='_sudo_systemctl_journal'
alias sduj='_systemctl_journal --user'

# alias journalu='journalctl -eu'

# alias start='sudo systemctl start'
# alias stop='sudo systemctl stop'
# alias restart='sudo systemctl restart'
# alias reload='sudo systemctl reload'
# alias status='systemctl status'

# alias startj='_sudo_systemctl_journal start'
# alias restartj='_sudo_systemctl_journal restart'
# alias reloadj='_sudo_systemctl_journal reload'
