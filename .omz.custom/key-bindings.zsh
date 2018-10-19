
# tested on urxvt
# also see ~/.inputrc

# Copy then deactivate region
copy-region-as-kill-deactivate() {
	zle copy-region-as-kill
	zle deactivate-region
}
zle -N copy-region-as-kill-deactivate

# If region is selected: "zsh copy" and "xclip copy"
# Else if no region: "xclip copy" the last "zsh copy"
copy-region-as-kill-xclip-deactivate() {
	zle copy-region-as-kill
	echo "$CUTBUFFER" | xclip -r
	zle deactivate-region
}
zle -N copy-region-as-kill-xclip-deactivate

# Motions
bindkey "\eOc" emacs-forward-word       # [Ctrl-Right]
bindkey "\eOd" emacs-backward-word      # [Ctrl-Left]
bindkey "\e[3^" delete-word             # [Ctrl-Del]
bindkey "\e[8^" end-of-line             # [Ctrl-End]
bindkey "\e[7^" beginning-of-line       # [Ctrl-Home]
bindkey '^I' complete-word				# [Ctrl-I]

# Quote region
bindkey "^[\'" quote-region				# [Meta-']

# Copy-Paste
bindkey '^G' deactivate-region							# [Ctrl-G]
bindkey '^W' kill-region								# [Ctrl-W]
bindkey '\e[2^' copy-region-as-kill-xclip-deactivate	# [Ctrl-Insert]
bindkey '^[w' copy-region-as-kill-deactivate			# [Meta-W]

# Enable Ctrl-Backspace only on supported term
if [[ "$TERM" = rxvt* ]]
then
	bindkey "^H" backward-delete-word   # [Ctrl-Backspace]
fi
