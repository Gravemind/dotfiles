
_systemctl_j() {
    sudo echo "$fg_bold[blue]Begin $@ & ...$reset_color"
    begin="$(date +'%F %T')"
    {
        sudo systemctl "$@"
        r="$?"
        if [[ "$r" -ne 0 ]] ; then
            echo "$fg_bold[blue]... End, $@ & $fg_bold[red]failed ($r)$fg_bold[blue] (Ctrl-C to quit)$reset_color"
        else
            echo "$fg_bold[blue]... End, $@ & $fg_bold[green]succeeded$fg_bold[blue] (Ctrl-C to quit)$reset_color"
        fi
    } &
    sleep 0.2
    journalctl -f --since "$begin"
}
compdef _systemctl_j='systemctl'

alias start='_systemctl_j start'
alias stop='_systemctl_j stop'
alias restart='_systemctl_j restart'
alias reload='_systemctl_j reload'

alias status='_ systemctl status'
alias journal='journalctl -u'
