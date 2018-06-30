#!/bin/zsh

#
# bare git dotfiles:
#
# https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/
# https://news.ycombinator.com/item?id=11071754
#

DOTFILES_ROOT="$HOME"
DOTFILES_BARE="$HOME/.dotfiles.git"

# toggles env GIT_DIR and GIT_WORK_TREE to home dotfiles git
# makes git dotfiles works in magit
function dotfiles() {
	if [[ -z "$GIT_DIR" && -z "$GIT_WORK_TREE" ]]
	then
		export GIT_DIR="$DOTFILES_BARE"
		export GIT_WORK_TREE="$DOTFILES_ROOT"
		echo "Git is now forced to $GIT_WORK_TREE ($GIT_DIR)"
	else
		if [[ "$GIT_DIR" != "$DOTFILES_BARE" && "$GIT_WORK_TREE" != "$DOTFILES_BARE" ]]
		then
			echo unknown current "$GIT_DIR" "$GIT_WORK_TREE"
			echo will do nothing
			exit 1
		fi
		unset GIT_DIR
		unset GIT_WORK_TREE
		echo "Git is back to normal"
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
		git ls-files | sed -E 's#(.*)/.*?#\1#g' | sort -u | xargs git s
	)
}
