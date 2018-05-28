#!/bin/zsh

function _onkill_killagent() {
	local selfpid=$$
	if [[ -n "$SSH_AGENT_PID" && -e "/proc/$SSH_AGENT_PID" && "$SSH_AGENT_OWNER_PID" == "$selfpid" ]]
	then
		kill $SSH_AGENT_PID
	fi
}

trap '_onkill_killagent' EXIT

function agent() {
	if [[ -n "$SSH_AGENT_PID" && -e "/proc/$SSH_AGENT_PID" && -e "$SSH_AUTH_SOCK" ]]
	then
		echo "agent already runing pid $SSH_AGENT_PID"
		return
	fi
	local selfpid=$$
	export SSH_AGENT_OWNER_PID="$selfpid"
	eval "$(ssh-agent -s)"
	ssh-add
}
