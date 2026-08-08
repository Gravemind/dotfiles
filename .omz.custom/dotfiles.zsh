#!/bin/zsh

#
# bare git dotfiles:
#
# ~/dotfiles/README.md
# https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/
# https://news.ycombinator.com/item?id=11071754
#

DOTFILES_ROOT="$HOME"
DOTFILES_BARE="$HOME/.dotfiles.git"

# $ dotfiles
# Toggles $HOME as git repo.
# Creates or removes the symlink ~/.git to ~/.dotfiles.git
# (works with magit (GIT_DIR not supported by magit anymore...))
# Note: could also be a regular file containing: 'gitdir: /home/user/.dotfiles.git'
function dotfiles() {
	local link="$HOME/.git"
	if [[ -h "$link" || -f "$line" ]]
	then
		rm "$link"
		echo "Home as git repo disabled: $link -> $DOTFILES_BARE link removed"
	elif [[ -e "$link" ]]
	then
		echo "ERROR: $link exists but not a symlink!"
		return 1
	else
		ln -s "$DOTFILES_BARE" "$link"
		echo "Home as git repo GLOBALLY enabled: $link -> $DOTFILES_BARE link created"
	fi
}

# $ dotc cmd...
# $ dotc git log
# Runs cmd... with env setup to force git to use HOME as git repo
# Note: will override/ignore any repo or submodule from current directory
function dotc() {
	(
		export GIT_DIR="$DOTFILES_BARE"
		export GIT_WORK_TREE="$DOTFILES_ROOT"
		"$@"
	)
}

# $ dots
# Runs git status on dotfiles git repo
function dots() {
	dotc git status -sb
}
