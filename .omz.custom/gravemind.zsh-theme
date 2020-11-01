#!/bin/zsh
#
# Gravemind theme
#
# Inspired by ~/.oh-my-zsh/themes/agnoster.zsh-theme
#
# zsh prompt expansion: http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
#

my_sourcing_again="${my_sourcing_again:-false}"
$my_sourcing_again || export _shell_depth="$((${_shell_depth:-0} + 1))"

GRAVEMIND_NO_URGENT_CMD+=( mpv steam )
GRAVEMIND_NO_URGENT=0

GRAVEMIND_DEFAULT_UMASK=0027
GRAVEMIND_DEFAULT_GROUP="$(id -gn $(id -un))"

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

gravemind_init_prompt_vars() {
	_GRAVEMIND_GIT=0
	if [[ "$(git config --get oh-my-zsh.hide-status 2>/dev/null)" != "1" ]]; then
		local repo_info
		repo_info="$(git rev-parse --show-cdup --git-dir 2>/dev/null)"
		if [[ $? -eq 0 ]]; then
			_GRAVEMIND_GIT=1
			local gitdir="${repo_info##*$'\n'}"
			repo_info="${repo_info%$'\n'*}"
			local cdup="${repo_info##*$'\n'}"
			repo_info="${repo_info%$'\n'*}"
			_GRAVEMIND_GIT_TOPLEVEL="$(cd "$cdup" && pwd)"
			_GRAVEMIND_GIT_GITDIR="$gitdir"
		fi
	fi
}

gravemind_prompt_user() {
	# user
	if [[ $EUID -ne 1000 || -n "$SSH_CONNECTION" ]]; then
		echo -n " %(!.%F{red}.%F{blue})%n"
	fi
	# @host
	if [[ -n "$SSH_CONNECTION" ]]; then
		echo -n "%F{green}@%m"
	fi
}

_gravemind_prompt_short_dir_default() {
	echo -n " %F{blue}%30>…>%1~%<<"
}

_gravemind_prompt_short_dir_project() {
	[[ $_GRAVEMIND_GIT -eq 1 ]] || return 1
	local toplevel="$_GRAVEMIND_GIT_TOPLEVEL"
	local pwd="$(pwd)"
	[[ "$pwd" == "$toplevel"* ]] || return 1
	# a/b/c where b is project top level dir
	local a="${toplevel%/*}"
	local b="${toplevel##*/}"
	local c="${${pwd#$toplevel}#/}"
	local trunc="${c##*/}"
	if [[ -n "$trunc" ]]; then
		if [[ "$trunc" != "$c" ]]; then
			trunc="%{…%G%}/$trunc"
		fi
		trunc="/$trunc"
	fi
	echo -n " %F{blue}$GMPSGIT %20>…>${b}%<<%F{black}%20>…>$trunc%<<"
}

gravemind_prompt_short_dir() {
	_gravemind_prompt_short_dir_project || _gravemind_prompt_short_dir_default
}

_gravemind_prompt_default_dir() {
	echo -n " %F{black}%~"
}

_gravemind_prompt_project_dir() {
	[[ $_GRAVEMIND_GIT -eq 1 ]] || return 1
	local toplevel="$_GRAVEMIND_GIT_TOPLEVEL"
	local pwd="$(pwd)"
	[[ "$pwd" == "$toplevel"* ]] || return 1
	# a/b/c where b is project top level dir
	local a="${toplevel%/*}"
	local b="${toplevel##*/}"
	local c="${pwd#$toplevel}"
	a="${a/#"$HOME"/~}"
	echo -n " %F{black}$a/%F{blue}$b%F{black}$c"
}

gravemind_prompt_dir() {
	_gravemind_prompt_project_dir || _gravemind_prompt_default_dir
}

gravemind_prompt_git_doing() {
	[[ $_GRAVEMIND_GIT -eq 1 ]] || return 1
	local g="$_GRAVEMIND_GIT_GITDIR"
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
		return 0
	fi
	echo -n " %F{red}$r"
}

gravemind_prompt_git() {
	local g
	if $GRAVEMIND_PROMPT_USE_GITFAST; then
		g="$(__git_ps1 "%s")"
	else
		g="$(git_current_branch)"
	fi
	[[ -z "$g" ]] || echo -n " %F{blue}$g"
}

gravemind_prompt_cc() {
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
	echo -n " $cc $progs"
}

gravemind_prompt_agent() {
	local o
	if [[ -n "$SSH_AUTH_SOCK" ]]; then
		ssh-add -l >& /dev/null
		if [[ $? == 0 || $? == 1 ]]; then ## returns 1 when connected but no keys
			local selfpid=$$
			if [[ "$SSH_AGENT_OWNER_PID" == "$selfpid" ]]; then # see ~/.omz.custom/agent.zsh
				o="%F{green}A"
			else
				o="%F{blue}A"
			fi
		else
			o="%F{red}A"
		fi
	else
		o="%F{black}A"
	fi
	[[ -z "$o" ]] || echo -n " $o"
}

_GRAVEMIND_CMD_START=-1
_GRAVEMIND_CMD_DURATION=-1
_GRAVEMIND_LAST_CMD=

# Called when the command is about to be executed
gravemind_preexec() {
	# local cmd="$1"

	# make sure prompt color doesn't leak (black background ?)
	echo -n $'\033[0m'

	_GRAVEMIND_LAST_CMD="$1"
	_GRAVEMIND_CMD_DURATION=-1
	_GRAVEMIND_CMD_START=$SECONDS
}

# Called once before each new prompt
gravemind_precmd() {
	local now=$SECONDS

	# Urgent alert after commands
	local urgent=1
	local last_cmd="$_GRAVEMIND_LAST_CMD"
	#echo "last $last_cmd"
	for exclude in "${GRAVEMIND_NO_URGENT_CMD[@]}"; do
		if [[ "$last_cmd" =~ ^"$exclude"$ ||
			  "$last_cmd" =~ ^"$exclude"" " ]]; then
			urgent=0
			break
		fi
	done
	[[ $urgent -eq 0 || $GRAVEMIND_NO_URGENT -eq 1 ]] || echo -n $'\a'

	if [[ $_GRAVEMIND_CMD_START -ge 0 && $_GRAVEMIND_CMD_DURATION -eq -1 ]]; then
		_GRAVEMIND_CMD_DURATION=$(($now - $_GRAVEMIND_CMD_START))

		# Urgent if command took more than 1sec
		# .Xresource: URxvt*urgentOnBell: true
		#[[ $_GRAVEMIND_CMD_DURATION -lt 1 ]] || echo -n $'\a'

	else
		_GRAVEMIND_CMD_DURATION=-1
		_GRAVEMIND_CMD_START=-1
	fi

	gravemind_init_prompt_vars
}

gravemind_prompt_cmd_duration() {
	local sec=$_GRAVEMIND_CMD_DURATION
	[[ $sec -gt 0 ]] || return 0
	[[ $sec -ge 1 ]] || return 0
	local r=
	[[ $sec -lt 3600 ]] || { r+="$(($sec / 3600))h"; sec=$(($sec % 3600)); }
	[[ $sec -lt 60 ]] || { r+="$(($sec / 60))m"; sec=$(($sec % 60)); }
	[[ $sec -lt 1 ]] || { r+="$(($sec))s"; }
	local c="%F{black}"
	[[ $sec -le 3 ]] || c="%F{yellow}"
	echo -n " $c↳$r↲"
}

gravemind_prompt_umask() {
	local umask="$(umask)"
	[[ "$umask" -ne "$GRAVEMIND_DEFAULT_UMASK" ]] || return 0
	echo -n " %F{magenta}$umask"
}

gravemind_prompt_group() {
	local group="$(id -gn)"
	[[ "$group" != "$GRAVEMIND_DEFAULT_GROUP" ]] || return 0
	echo -n " %F{magenta}$group"
}

gravemind_build_prompt() {
	echo -n "%K{black}%B"
	if [[ "$_shell_depth" -gt 1 ]]; then
		for i in {2..$_shell_depth}
		do
			echo -n "%F{yellow}$GMPSBEG"
		done
	fi
	echo -n "%F{white}$GMPSBEG" # prefix
	gravemind_prompt_user
	gravemind_prompt_short_dir
	echo -n "%1(j. %F{yellow}%j.)" # jobs
	echo -n "%(?.. %F{red}%?)" # error status
	gravemind_prompt_git_doing
	echo -n " %B%F{white}$GMPSEND" # suffix
	echo -n " "
}

gravemind_build_rprompt() {
	echo -n "%K{black}%B"
	echo -n "%F{white}$GMPSBEG" # prefix
	gravemind_prompt_cmd_duration
	gravemind_prompt_umask
	gravemind_prompt_group
	gravemind_prompt_git
	gravemind_prompt_dir
	gravemind_prompt_cc
	gravemind_prompt_agent
	echo -n " %F{black}%D{%H:%M}" # time
	echo -n " %B%F{white}$GMPSEND" # suffix
}

GRAVEMIND_PROMPT_USE_GITFAST=true
if $GRAVEMIND_PROMPT_USE_GITFAST; then
	#GIT_PS1_SHOWDIRTYSTATE=true ## a bit slow
	#GIT_PS1_SHOWCOLORHINTS=true
	#GIT_PS1_SHOWUNTRACKEDFILES=true ## quite slow
	#GIT_PS1_SHOWUPSTREAM="verbose"
	source $ZSH/plugins/gitfast/git-prompt.sh
fi

PROMPT='%f%b%K{black}$(gravemind_build_prompt)%f%b%K{black}'

RPROMPT='%f%b%K{black}$(gravemind_build_rprompt)%f%b%k'

PROMPT2="%K{black}%B  %F{blue}%_ %F{white}$GMPSEND %f%b%K{black}"

## prepend
if ! $my_sourcing_again; then
	preexec_functions+=(gravemind_preexec)
	precmd_functions=(gravemind_precmd "${precmd_functions[@]}")
fi

## refresh prompt every 60 sec
## builtin TRAPALRM called every TMOUT
## http://stackoverflow.com/questions/13125825/zsh-update-prompt-with-current-time-when-a-command-is-started
TMOUT=$(( 5 + 60 - $(date '+%s') % 60 ))
TRAPALRM() {
	zle reset-prompt
	TMOUT=$(( 5 + 60 - $(date '+%s') % 60 ))
}

my_sourcing_again=true
