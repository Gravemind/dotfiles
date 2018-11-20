#!/bin/zsh

export LESS_TERMCAP_mb="${fg_bold[red]}"	# begin blinking (?)
export LESS_TERMCAP_md="${fg_bold[blue]}"   # begin bold (titles)
export LESS_TERMCAP_me=$'\E[0m'				# end mode
export LESS_TERMCAP_so="${fg_bold[white]}"  # begin standout-mode (search)
#export LESS_TERMCAP_so=$'\E[7m'  # begin standout-mode (search)
export LESS_TERMCAP_se=$'\E[0m'				# end standout-mode
export LESS_TERMCAP_us="${fg_bold[cyan]}"   # begin underline (words)
export LESS_TERMCAP_ue=$'\E[0m'				# end underline

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
# (-J : --status-column)
# -jn : --jump-target=n : linenum position of "jumped to" target
# -zn : --window=n : PAGE-up/down scrolled lines, negative for win_height-n
export LESS='-qRiSM -j3 -z-4'

export MANPAGER='less -J'

# for systemd (journalctl...)
export SYSTEMD_LESS="$LESS"

alias lesss='less -+S'

alias -g LESS='|& less'
alias -g LESSS='|& less -+S'
