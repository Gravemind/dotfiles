
# ATL-R - Paste the selected command *argument* from history into the command line
# FIXME: \n in arguments
fzf-history-arg-widget() {
  local selected
  setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases noglob nobash_rematch 2> /dev/null
  selected="$({echo $LBUFFER; fc -rnl 1;} | while read -r l; do args=(${(z)l}); printf '%s\n' "${(Oa)args[@]}"; done | awk 'length($0) <= 2 {next;} { if(!seen[$0]++) print $0 }' |
	FZF_DEFAULT_OPTS=$(__fzf_defaults "" "-n2..,.. --scheme=history --bind=ctrl-r:toggle-sort --wrap-sign '\t↳ ' --highlight-line ${FZF_ALT_R_OPTS-} +m") \
	FZF_DEFAULT_OPTS_FILE='' $(__fzfcmd))"
  local ret=$?
  LBUFFER="${LBUFFER% } ${selected% }"
  zle reset-prompt
  return $ret
}
zle     -N             fzf-history-arg-widget
bindkey -M emacs '\er' fzf-history-arg-widget
bindkey -M vicmd '\er' fzf-history-arg-widget
bindkey -M viins '\er' fzf-history-arg-widget
