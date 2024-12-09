#!/bin/zsh

export LESS_TERMCAP_mb=$'\e[01;31m' # begin blinking (?)
export LESS_TERMCAP_md=$'\e[01;34m' # begin bold (titles)
export LESS_TERMCAP_me=$'\e[0m'     # end mode
export LESS_TERMCAP_so=$'\e[01;37m' # begin standout-mode (search)
#export LESS_TERMCAP_so=$'\E[7m'    # begin standout-mode (search)
export LESS_TERMCAP_se=$'\e[0m'     # end standout-mode
export LESS_TERMCAP_us=$'\e[01;36m' # begin underline (words)
export LESS_TERMCAP_ue=$'\e[0m'     # end underline

# Fix man colors on Konsole, Gnome-terminal, alacritty...
export GROFF_NO_SGR=1  # all grotty
# export MANROFFOPT=-P-c  # only man-pages

export LESSBINFMT='*u<%02X>'

# -q  : --quiet : less bell ringing
# (-F : --quit-if-one-screen)
# -R  : --RAW-CONTROL-CHARS: allow **only** color termcap escape sequences
# (-r : --raw-control-chars)
# (-X : --no-init : no termcap clear on quit)
# -i  : --ignore-case : smart ignore case
# (-I : --IGNORE-CASE : always ignore case)
# -S  : --chop-long-lines : truncate lines (vs wrap lines)
# -M  : --LONG-PROMPT : longest prompt
# (-m : --long-prompt)
# (-J : --status-column) : left-margin to highlight search/find
# -jn : --jump-target=n : linenum position of "jumped to" target
# -zn : --window=n : PAGE-up/down scrolled lines, negative for win_height-n
export LESS='-qRiSM -j5 -z-4'

function man() {
	# `less -J` adds a 2 char-wide search result marker before each lines, so we
	# need to reduce man's width to account for that.
	MANPAGER="less -J" MANWIDTH="$((COLUMNS - 2))" command man "$@"
}

# for systemd (journalctl...)
export SYSTEMD_LESS="$LESS -F"

alias lesss='less -+S'

alias -g LESS='|& less'
alias -g LESSS='|& less -+S'
