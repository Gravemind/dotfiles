#!/bin/zsh

## http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html

HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='fg=red,bold'

PROMPT_USER=""
# level (shell recursion depth)
export OMZLVL="$((${OMZLVL:--1} + 1))"
if [[ "$OMZLVL" -gt 0 ]]; then
	#PROMPT_USER="%F{yellow}$OMZLVL %F{white}:"
	for i in {1..$OMZLVL}
	do
		PROMPT_USER+="%F{yellow}%{❰%G%}"
	done
fi
# user
if [[ $EUID -ne 1000 || -n "$SSH_CONNECTION" ]]; then
	PROMPT_USER="${PROMPT_USER} %(!.%F{red}.%F{blue})%n"
fi
# @host
if [[ -n "$SSH_CONNECTION" ]]; then
	PROMPT_USER="${PROMPT_USER}%F{green}@$HOST"
fi
# end separator
if [[ -n "$PROMPT_USER" ]]; then
	PROMPT_USER="$PROMPT_USER %F{white}%{∶%G%}"
fi

PROMPT='%K{black}%B%F{white}%{❰%G%}'"${PROMPT_USER}"' $(gravemind_prompt_info_short)%1(j. %F{white}%{∶%G%} %F{green}%j.)%(?.. %F{white}%{∶%G%} %F{red}%?)$(gravemind_git_doing) %F{white}%{❱%G%} %f%b%K{black}'

RPS1='%K{black}%B%F{white}%{❰%G%}$GRAVEMIND_PROMPT_CMD_TIME $(gravemind_prompt_info_long) %F{white}%{∶%G%} $(gravemind_promt_cc)%F{white}%{∶%G%} %F{blue}%D{%H:%M} %F{white}%{❱%G%}%f%b%k'

PROMPT2='%K{black}%B  %F{blue}%_ %F{white}%{❱%G%} %f%b%k'

unset PROMPT_USER

GRAVEMIND_PROMPT_USE_GITFAST=true
if $GRAVEMIND_PROMPT_USE_GITFAST
then
	#GIT_PS1_SHOWDIRTYSTATE=true ## a bit slow
	#GIT_PS1_SHOWCOLORHINTS=true
	#GIT_PS1_SHOWUNTRACKEDFILES=true ## quite slow
	#GIT_PS1_SHOWUPSTREAM="verbose"
	source $ZSH/plugins/gitfast/git-prompt.sh
fi

function gravemind_git_doing() {
	## .oh-my-zsh/plugins/gitfast/git-prompt.sh
	$GRAVEMIND_PROMPT_USE_GITFAST || return 0;
	local g="$(command git rev-parse --git-dir 2> /dev/null)"
	[[ -n "$g" ]] || return 0;
	local r=
	if [[ -e "$g/rebase-merge/interactive" ]]; then
		r="REBASING-i"
	elif [[ -e "$g/rebase-merge" ]]; then
		r="REBASING-m"
	elif [[ -e "$g/rebase-apply/rebasing" ]]; then
		r="REBASING"
	elif [[ -e "$g/rebase-apply/applying" ]]; then
		r="AMING"
	elif [[ -e "$g/rebase-apply" ]]; then
		r="AMING/REBASING"
	elif [[ -e "$g/MERGE_HEAD" ]]; then
		r="MERGING"
	elif [[ -e "$g/CHERRY_PICK_HEAD" ]]; then
		r="CHERRY-PICKING"
	elif [[ -e "$g/REVERT_HEAD" ]]; then
		r="REVERTING"
	elif [[ -e "$g/BISECT_LOG" ]]; then
		r="BISECTING"
	else
		return 0;
	fi
	echo -n " %F{white}%{∶%G%} %F{red}$r"
}

function gravemind_git_prompt_current_branch() {
	if $GRAVEMIND_PROMPT_USE_GITFAST
	then
		__git_ps1 "%s"
	else
		git_current_branch
	fi
}

function gravemind_prompt_info_short() {
	local toplevel="$(command git rev-parse --show-toplevel 2>/dev/null)"
	if [[ -z "$toplevel" ]]; then
		echo -n "%F{blue}%30>…>%1~%<<"
		return
	fi
	if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" = "1" ]]; then
		echo -n "%F{blue}%30>…>%1~%<<"
		return
	fi
	## FIXME when pwd is outside toplevel
	local gitrootname="$(basename "$toplevel")"
	local subgit="${$(pwd)#$toplevel}"
	local shortsubgit=""
	if [[ -z "$subgit" ]]; then
		shortsubgit=""
	else
		local subgitlast="$(basename "$subgit")"
		if [[ "$subgit" = "/$subgitlast" ]]; then
			shortsubgit="/$subgitlast"
		else
			shortsubgit="/%{…%G%}/$subgitlast"
		fi
	fi
	#local giticon=""
	local giticon="⌥"
	#local giticon=""
	#local giticon=""
	echo -n "%F{blue}$giticon %20>…>$gitrootname%<<%F{black}%20>…>$shortsubgit%<</"
}

function gravemind_prompt_info_long() {
	local toplevel="$(command git rev-parse --show-toplevel 2>/dev/null)"
	if [[ -z "$toplevel" ]]; then
		echo -n "%F{black}%~"
		return
	fi
	if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" = "1" ]]; then
		echo -n "%F{black}%~ %F{white}%{∶%G%} %F{black}(oh-my-zsh.hide-status)"
		return
	fi
	## FIXME when pwd is outside toplevel
	local toplevelbase="$(basename "$toplevel")"
	local toplevelparent="$(dirname "$toplevel")/"
	local pwdbegin="${toplevelparent/#$HOME\//~/}"
	local pwdgit="$toplevelbase"
	local pwdend="${$(pwd)#$toplevel}"
	echo -n "%F{blue}$(gravemind_git_prompt_current_branch) %F{white}%{∶%G%} %F{black}$pwdbegin%F{blue}$pwdgit%F{black}$pwdend"
}

function gravemind_promt_cc() {
	local cc
	if [[ -n "$CC" ]]; then
		local ccname="$(basename "$CC")"
		cc="%F{blue}$ccname"
	else
		cc="%F{black}CC"
	fi
	local progs=""
	if [[ "$ENABLE_RTAGS" == "1" ]]; then
		progs+="%F{blue}R"
	else
		progs+="%F{black}R"
	fi
	if [[ "$ENABLE_CCACHE" == "1" ]]; then
		progs+="%F{blue}C"
	else
		progs+="%F{black}C"
	fi
	if [[ -n "$SSH_AUTH_SOCK" ]]
	then
		ssh-add -l >& /dev/null
		if [[ $? == 0 || $? == 1 ]] ## returns 1 when connected but no keys
		then
			local selfpid=$$
			if [[ "$SSH_AGENT_OWNER_PID" == "$selfpid" ]]; then
				progs+="%F{green}A"
			else
				progs+="%F{blue}A"
			fi
		else
			progs+="%F{red}A"
		fi
	else
		progs+="%F{black}A"
	fi
	echo -n "$cc $progs "
}

GRAVEMIND_CMD_START=$SECONDS
function gravemind_preexec() {
	# make sure prompt color doesn't leak (black background ?)
	echo -n $'\033[0m'
	GRAVEMIND_CMD_START=$SECONDS
}

GRAVEMIND_PROMPT_CMD_TIME=""
function gravemind_precmd() {
	if [[ $GRAVEMIND_CMD_START -lt 0 ]]
	then
		GRAVEMIND_PROMPT_CMD_TIME=""
		return
	fi
	last_cmd_time=$(($SECONDS - $GRAVEMIND_CMD_START))
	GRAVEMIND_CMD_START=-1
	if [[ $last_cmd_time -ge 1 ]]
	then
		## .Xresource: URxvt*urgentOnBell: true
		echo -ne '\a'
		GRAVEMIND_PROMPT_CMD_TIME=" %F{blue}↳"
		[[ $last_cmd_time -lt 3600 ]] || { GRAVEMIND_PROMPT_CMD_TIME+="$(($last_cmd_time / 3600))h"; last_cmd_time=$(($last_cmd_time % 3600)) }
		[[ $last_cmd_time -lt 60 ]] || { GRAVEMIND_PROMPT_CMD_TIME+="$(($last_cmd_time / 60))m"; last_cmd_time=$(($last_cmd_time % 60)) }
		[[ $last_cmd_time -lt 1 ]] || { GRAVEMIND_PROMPT_CMD_TIME+="$(($last_cmd_time))s"; }
		GRAVEMIND_PROMPT_CMD_TIME+="↲ %F{white}%{∶%G%}"
	else
		GRAVEMIND_PROMPT_CMD_TIME=""
	fi
}

## prepend
preexec_functions+=(gravemind_preexec)
precmd_functions=(gravemind_precmd "${precmd_functions[@]}")

## refresh prompt every 60 sec
## builtin TRAPALRM called every TMOUT
## http://stackoverflow.com/questions/13125825/zsh-update-prompt-with-current-time-when-a-command-is-started
TMOUT=$(( 5 + 60 - $(date '+%s') % 60 ))
TRAPALRM() {
	zle reset-prompt
	TMOUT=$(( 5 + 60 - $(date '+%s') % 60 ))
}
