
# tested on urxvt
# also see ~/.inputrc

# emacs
bindkey "\eOc" emacs-forward-word       # [Ctrl-Right]
bindkey "\eOd" emacs-backward-word      # [Ctrl-Left]
bindkey "\e[3^" delete-word             # [Ctrl-Del]
bindkey "\e[8^" end-of-line             # [Ctrl-End]
bindkey "\e[7^" beginning-of-line       # [Ctrl-Home]
bindkey '^W' kill-region				# [Ctrl-W]
bindkey '^G' deactivate-region			# [Ctrl-G]
bindkey '^I' complete-word				# [Ctrl-I]

copy-and-deactivate-region() {
	zle copy-region-as-kill
	zle deactivate-region
}
zle -N copy-and-deactivate-region
bindkey '^[w' copy-and-deactivate-region # [Meta-W]

if [[ "$TERM" = rxvt* ]]
then
	bindkey "^H" backward-delete-word   # [Ctrl-Backspace]
fi

# other
bindkey "^[\'" quote-region				# [Ctrl-']
bindkey '^["' quote-line				# [Ctrl-"]
