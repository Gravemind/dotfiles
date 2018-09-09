#!/bin/zsh

#
# bare git dotfiles:
#
# https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/
# https://news.ycombinator.com/item?id=11071754
#

DOTFILES_ROOT="$HOME"
DOTFILES_BARE="$HOME/.dotfiles.git"

# creates a symlink ~/.git to ~/.dotfiles.git
# (works with magit (GIT_DIR not supported by magit anymore...))
function dotfiles() {
	local link="$HOME/.git"
	if [[ -h "$link" ]]
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

# alias git
alias dot="git --git-dir=$DOTFILES_BARE --work-tree=$DOTFILES_ROOT"

# dotfiles command (dotc emacs)
function dotc() {
	(
		export GIT_DIR="$DOTFILES_BARE"
		export GIT_WORK_TREE="$DOTFILES_ROOT"
		"$@"
	)
}

# dotfiles status
function dots() {
	(
		export GIT_DIR="$DOTFILES_BARE"
		export GIT_WORK_TREE="$DOTFILES_ROOT"
		git -c status.showUntrackedFiles=normal ls-files |
			sed -E 's#(.*)/.*?#\1#g' |
			sort -u |
			xargs git -c status.showUntrackedFiles=normal s
	)
}
