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
	local selfpid=$$
	ssh-add -l >& /dev/null
	if [[ $? == 0 || $? == 1 ]] ## returns 1 when connected but no keys
	then
		echo "agent already running"
	else
		export SSH_AGENT_OWNER_PID="$selfpid"
		eval "$(ssh-agent -s)"
	fi
	if [[ "$1" = "a" ]]
	then
		ssh-add
	fi
}
