#!/bin/zsh

function dotfiles() {

	ROOT="$HOME"
	BARE="$HOME/.dotfiles.git"

	if [[ -z "$GIT_DIR" && -z "$GIT_WORK_TREE" ]]
	then
		export GIT_DIR="$BARE"
		export GIT_WORK_TREE="$ROOT"
		echo "git dotfiles are now ON"
	else
		if [[ "$GIT_DIR" != "$BARE" && "$GIT_WORK_TREE" != "$BARE" ]]
		then
			echo unknown current "$GIT_DIR" "$GIT_WORK_TREE"
			echo will do nothing
			exit 1
		fi
		unset GIT_DIR
		unset GIT_WORK_TREE
		echo "git dotfiles are now OFF"
	fi
}
