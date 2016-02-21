#!/bin/zsh

# toggles env GIT_DIR and GIT_WORK_TREE to home dotfiles git
# makes git dotfiles works in magit
function dotfiles() {
	ROOT="$HOME"
	BARE="$HOME/.dotfiles.git"

	if [[ -z "$GIT_DIR" && -z "$GIT_WORK_TREE" ]]
	then
		export GIT_DIR="$BARE"
		export GIT_WORK_TREE="$ROOT"
		echo "Git is now forced to $GIT_WORK_TREE ($GIT_DIR)"
	else
		if [[ "$GIT_DIR" != "$BARE" && "$GIT_WORK_TREE" != "$BARE" ]]
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
