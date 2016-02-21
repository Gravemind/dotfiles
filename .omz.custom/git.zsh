#!/bin/zsh

alias cdr='cd "$(git rev-parse --show-toplevel)"'

# overwrite git plugin
alias gdc=gdc

function gitsetuser() {
	local name="$1"
	local email="$2"
	[[ -n "$name" && -n "$email" ]] || { echo "INVALID name email"; return 1 }

	git config --local user.name "$1"
	git config --local user.email "$2"

	git config user.name
	git config user.email
}
