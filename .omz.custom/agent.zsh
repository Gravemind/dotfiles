#!/bin/zsh

function _onkill_killagent() {
	if [[ -n "$SSH_AGENT_PID" ]]
	then
		kill $SSH_AGENT_PID
	fi
}

trap '_onkill_killagent' EXIT

function agent() {
	if [[ -n "$SSH_AGENT_PID" ]] && kill -0 "$SSH_AGENT_PID"
	then
		echo "agent already runing pid $SSH_AGENT_PID"
		return
	fi
	eval "$(ssh-agent -s)"
	ssh-add
}
