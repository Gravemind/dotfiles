#!/bin/zsh

#
# Search and insert a previous argument from history
#
# Alt-R
#
# FIXME: \n in arguments
fzf-history-arg-widget() {
  local selected
  selected="$(_history_args |
	FZF_DEFAULT_OPTS=$(__fzf_defaults "" "-n2..,.. --scheme=history --bind=ctrl-r:toggle-sort --wrap-sign '\t↳ ' --highlight-line ${FZF_ALT_R_OPTS-} +m") \
	FZF_DEFAULT_OPTS_FILE='' $(__fzfcmd))"
  local ret=$?
  LBUFFER="${LBUFFER% } ${selected% } "
  zle reset-prompt
  return $ret
}
_history_args() {
	local min_arg_width=4
	# Process history in batches:
	# - lower total time compared to single line processing
	# - lower latency of first entry compared to full history processing
	local batch_size=500  # Good compromise
	local hist batch b=$batch_size i=1
	while true
	do
		# Get a history batch, in reverse order, add current command (LBUFFER)
		hist="$(fc 2>/dev/null -nl -$((i + b - 1)) -$((i)); [[ $i != 1 ]] || print -rl -- "" "$LBUFFER")"
		[[ -n "$hist" ]] || break
		batch=(${(f)hist})         # Lines to array
		batch=(${(z)batch[@]})     # Split args (keeps quoting), for each array elements
		batch=(${(Oa)batch})       # Reverse array
		print -rl -- "${batch[@]}" # Print each element on new line
		i=$((i + b))
	done | awk 'length($0) < '"$min_arg_width"' {next;} { if(!seen[$0]++) print $0 }'
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
	newprompt="$(:| $(__fzfcmd) --print-query --multi --preview="$shell" --preview-window=down:99% --query="$prompt" --prompt="$SHELL > " --bind='up:preview-up,down:preview-down,page-up:preview-page-up,page-down:preview-page-down')"
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

#
# Bind Ctrl-arrow/delete/backspace
#
# FIXME: ctrl-backspace doesn't work (termio issue ?)
export FZF_DEFAULT_OPTS="--exact --no-sort --smart-case --bind='ctrl-left:backward-word,ctrl-right:forward-word,ctrl-delete:kill-word,ctrl-backspace:backward-kill-word'"
