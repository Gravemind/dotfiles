#!/bin/zsh

export OMZLVL="$((${OMZLVL:--1} + 1))"

## http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html

HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='fg=red,bold'

# GraveMind PS SEParator/BEGin/END
GMPSSEP="%F{white}%{∶%G%}"
GMPSBEG="%{❰%G%}"
GMPSEND="%{❱%G%}"
# Git icon
#GMPSGIT="%{%G%}"
GMPSGIT="%{⌥%G%}"
#GMPSGIT="%{%G%}"
#GMPSGIT="%{%G%}"

function gravemind_prompt_user() {
	local user=""
	# level/shell recursion depth
	if [[ "$OMZLVL" -gt 0 ]]; then
		#user="%F{yellow}$OMZLVL %F{white}:"
		for i in {1..$OMZLVL}
		do
			user+="%F{yellow}$GMPSBEG"
		done
	fi
	# user
	if [[ $EUID -ne 1000 || -n "$SSH_CONNECTION" ]]; then
		user="${user} %(!.%F{red}.%F{blue})%n"
	fi
	# @host
	if [[ -n "$SSH_CONNECTION" ]]; then
		user="${user}%F{green}@$HOST"
	fi
	[[ -n "$user" ]] || return 0;
	echo -n "$user $GMPSSEP"
}

# (`\$` will be eval at each prompt [re]draw)

PROMPT="%K{black}%B%F{white}$GMPSBEG$(gravemind_prompt_user) \$(gravemind_prompt_info_short)%1(j. $GMPSSEP %F{green}%j.)%(?.. $GMPSSEP %F{red}%?)\$(gravemind_git_doing) %F{white}$GMPSEND %f%b%K{black}"

RPS1="%K{black}%B%F{white}$GMPSBEG\$(gravemind_prompt_cmd_time) \$(gravemind_prompt_info_long) $GMPSSEP \$(gravemind_promt_cc)$GMPSSEP %F{blue}%D{%H:%M} %F{white}$GMPSEND%f%b%k"

PROMPT2="%K{black}%B  %F{blue}%_ %F{white}$GMPSEND %f%b%k"

GRAVEMIND_PROMPT_USE_GITFAST=true
if $GRAVEMIND_PROMPT_USE_GITFAST; then
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
	echo -n " $GMPSSEP %F{red}$r"
}

function gravemind_git_prompt_current_branch() {
	if $GRAVEMIND_PROMPT_USE_GITFAST; then
		__git_ps1 "%s"
	else
		git_current_branch
	fi
}

function gravemind_prompt_info_short() {
	local toplevel
	toplevel="$(command git rev-parse --show-cdup 2>/dev/null)"
	if [[ "$?" -ne 0 ]]; then
		echo -n "%F{blue}%30>…>%1~%<<"
		return
	fi
	toplevel="$(cd "./$toplevel" ; pwd)"
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
	echo -n "%F{blue}$GMPSGIT %20>…>$gitrootname%<<%F{black}%20>…>$shortsubgit%<</"
}

function gravemind_prompt_info_long() {
	local toplevel
	toplevel="$(command git rev-parse --show-cdup 2>/dev/null)"
	if [[ "$?" -ne 0 ]]; then
		echo -n "%F{black}%~"
		return
	fi
	toplevel="$(cd "./$toplevel" ; pwd)"
	if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" = "1" ]]; then
		echo -n "%F{black}%~ $GMPSSEP %F{black}(oh-my-zsh.hide-status)"
		return
	fi
	## FIXME when pwd is outside toplevel
	local toplevelbase="$(basename "$toplevel")"
	local toplevelparent="$(dirname "$toplevel")/"
	local pwdbegin="${toplevelparent/#$HOME\//~/}"
	local pwdgit="$toplevelbase"
	local pwdend="${$(pwd)#$toplevel}"
	echo -n "%F{blue}$(gravemind_git_prompt_current_branch) $GMPSSEP %F{black}$pwdbegin%F{blue}$pwdgit%F{black}$pwdend"
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
	if [[ -n "$SSH_AUTH_SOCK" ]]; then
		ssh-add -l >& /dev/null
		if [[ $? == 0 || $? == 1 ]]; then ## returns 1 when connected but no keys
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

GRAVEMIND_CMD_START=-1
GRAVEMIND_CMD_TIME=-1
GRAVEMIND_LAST_CMD=

# Called when the command is about to be executed
function gravemind_preexec() {
	# local cmd="$1"

	# make sure prompt color doesn't leak (black background ?)
	echo -n $'\033[0m'

	GRAVEMIND_LAST_CMD="$1"
	GRAVEMIND_CMD_TIME=-1
	GRAVEMIND_CMD_START=$SECONDS
}

GRAVEMIND_NO_URGENT_CMD+=( mpv steam )

GRAVEMIND_NO_URGENT=0

# Called once before each new prompt
function gravemind_precmd() {
	local now=$SECONDS

	# Urgent alert after commands
	local urgent=1
	local last_cmd="$GRAVEMIND_LAST_CMD"
	#echo "last $last_cmd"
	for exclude in "${GRAVEMIND_NO_URGENT_CMD[@]}"; do
		if [[ "$last_cmd" =~ ^"$exclude"$ ||
			  "$last_cmd" =~ ^"$exclude"" " ]]; then
			urgent=0
			break
		fi
	done
	[[ $urgent -eq 0 || $GRAVEMIND_NO_URGENT -eq 1 ]] || echo -n $'\a'

	if [[ $GRAVEMIND_CMD_START -ge 0 && $GRAVEMIND_CMD_TIME -eq -1 ]]; then
		GRAVEMIND_CMD_TIME=$(($now - $GRAVEMIND_CMD_START))

		# Urgent if command took more than 1sec
		# .Xresource: URxvt*urgentOnBell: true
		#[[ $GRAVEMIND_CMD_TIME -lt 1 ]] || echo -n $'\a'

	else
		GRAVEMIND_CMD_TIME=-1
		GRAVEMIND_CMD_START=-1
	fi
}

function gravemind_prompt_cmd_time() {
	[[ $GRAVEMIND_CMD_TIME -gt 0 ]] || return 0;
	local sec=$GRAVEMIND_CMD_TIME
	[[ $sec -ge 1 ]] || return 0;
	local r=
	[[ $sec -lt 3600 ]] || { r+="$(($sec / 3600))h"; sec=$(($sec % 3600)); }
	[[ $sec -lt 60 ]] || { r+="$(($sec / 60))m"; sec=$(($sec % 60)); }
	[[ $sec -lt 1 ]] || { r+="$(($sec))s"; }
	echo -n " %F{blue}↳$r↲ $GMPSSEP"
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
