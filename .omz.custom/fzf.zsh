#!/bin/zsh

#
# Search and insert a previous argument from history
#
# Alt-R
#
# FIXME: \n in arguments
fzf-history-arg-widget() {
  local selected
  selected="$({echo $LBUFFER; fc -rnl 1;} | while read -r l; do args=(${(z)l}); printf '%s\n' "${(Oa)args[@]}"; done | awk 'length($0) <= 2 {next;} { if(!seen[$0]++) print $0 }' |
	FZF_DEFAULT_OPTS=$(__fzf_defaults "" "-n2..,.. --scheme=history --bind=ctrl-r:toggle-sort --wrap-sign '\t↳ ' --highlight-line ${FZF_ALT_R_OPTS-} +m") \
	FZF_DEFAULT_OPTS_FILE='' $(__fzfcmd))"
  local ret=$?
  LBUFFER="${LBUFFER% } ${selected% } "
  zle reset-prompt
  return $ret
}
zle     -N             fzf-history-arg-widget
bindkey -M emacs '\er' fzf-history-arg-widget
bindkey -M vicmd '\er' fzf-history-arg-widget
bindkey -M viins '\er' fzf-history-arg-widget

#
# Run the current prompt with live output in fzf
#
# Alt-I
#
fzf-live-preview-widget() {
	local prompt="$BUFFER"
	local shell="${(qqq)SHELL} -c {q}"
	local newprompt
	newprompt="$(:| $(__fzfcmd) --print-query --multi --preview="$shell" --preview-window=down:99% --query="$prompt" --prompt="$SHELL > ")"
	local r=$?
	if [[ $r -eq 0 || $r -eq 1 ]]
	then
		BUFFER="$newprompt"
		CURSOR=$#BUFFER
	fi
	return 0
}
zle     -N             fzf-live-preview-widget
bindkey -M emacs '\ei' fzf-live-preview-widget
bindkey -M vicmd '\ei' fzf-live-preview-widget
bindkey -M viins '\ei' fzf-live-preview-widget
