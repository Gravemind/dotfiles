
#
# Cheat sheet:
#
# - show bindings
#   $ bindkey
#   $ bindkey -M emacs
#
# - show key stroke symbol
#   $ showkey -a
#
# - list commands
#   $ zle -al
#

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

function _my-zle-noop {  ; }
zle -N _my-zle-noop

bindkey -M vicmd 'Y' vi-yank-eol

for b in emacs viins vicmd
do
	# Motions
	bindkey -M $b "\eOc" emacs-forward-word       # [Ctrl-Right]
	bindkey -M $b "\eOd" emacs-backward-word      # [Ctrl-Left]
	bindkey -M $b "\e[8^" end-of-line             # [Ctrl-End]
	bindkey -M $b "\e[7^" beginning-of-line       # [Ctrl-Home]
	# '^I' = TAB, default is expand-or-complete
	# bindkey -M $b '^I' complete-word			  # [Ctrl-I]

	# Quote region
	bindkey -M $b "^[\'" quote-region			  # [Meta-']

	# Copy-Paste
	bindkey -M $b '^G' deactivate-region					    # [Ctrl-G]
	bindkey -M $b '^W' kill-region								# [Ctrl-W]
	bindkey -M $b '\e[2^' copy-region-as-kill-xclip-deactivate	# [Ctrl-Insert]
	bindkey -M $b '^[w' copy-region-as-kill-deactivate			# [Meta-W]

	bindkey -M $b "\e[3^" delete-word             # [Ctrl-Del]
	# Enable Ctrl-Backspace only on supported term
	case "$TERM"
	in
		 rxvt*|xterm*|alacritty|foot)
			 bindkey -M $b "^H" backward-delete-word  # [Ctrl-Backspace]
			 ;;
	esac

	# bindkey -M $b '^_' undo					      # [Ctrl-/]
	# bindkey -M $b '^R' redo						  # [Ctrl-R]

	# vi-mode patch
	bindkey -M $b '^[h' run-help

	# vi-mode patch: unbind
	bindkey -M $b '^[Oa' _my-zle-noop # [Ctrl-Up]
	bindkey -M $b '^[Ob' _my-zle-noop # [Ctrl-Down]

	# Remove the current prompt, and put it back after executing a command
	bindkey -M $b '^[q' push-input # [Alt-Q]
done
