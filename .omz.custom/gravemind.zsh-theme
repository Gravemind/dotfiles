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

GRAVEMIND_NO_GIT=0

GRAVEMIND_NO_URGENT_CMD+=( mpv steam )
GRAVEMIND_NO_URGENT=0

GRAVEMIND_DEFAULT_USER_ID=1000
GRAVEMIND_DEFAULT_GROUP="$(id -gn $(id -un))" # name
GRAVEMIND_DEFAULT_UMASK=0027

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
GMPSGITDIRTY="M"
GMPSTRUNC="%{…%G%}"
GMPSTRUNC1="…"
GMPSDUR1="%{↳%G%}"
GMPSDUR2="%{↲%G%}"

gravemind_init_prompt_vars() {
	_GRAVEMIND_GIT=0
	[[ "$GRAVEMIND_NO_GIT" != "1" ]] || return 0
	# [[ "$(git config --bool --get oh-my-zsh.hide-status 2>/dev/null)" != "true" ]] || return 0
	local info
	IFS=$'\n' info=(
		$(git rev-parse --git-dir HEAD --symbolic-full-name HEAD --show-cdup 2>/dev/null)
	)
	unset IFS
	# echol "<<<" "${info[@]}" ">>>>"
	[[ -n "${info[1]}" ]] || return 0
	_GRAVEMIND_GIT=1
	_GRAVEMIND_GIT_GITDIR="${info[1]}" # 1-indexed !?
	_GRAVEMIND_GIT_HEAD_SHA="${info[2]}"
	_GRAVEMIND_GIT_HEAD_REF="${info[3]}"
	if [[ "$PWD" == "$_GRAVEMIND_GIT_GITDIR" || "$PWD" == "$_GRAVEMIND_GIT_GITDIR"/* ]]; then
		_GRAVEMIND_GIT_TOPLEVEL="$_GRAVEMIND_GIT_GITDIR"
	else
		_GRAVEMIND_GIT_TOPLEVEL="$(cd "${info[4]:-}" && pwd)"
	fi
	_GRAVEMIND_GIT_HEAD_REF="${info[3]}"
}

gravemind_prompt_user() {
	# user
	if [[ $EUID -ne $GRAVEMIND_DEFAULT_USER_ID || -n "$SSH_CONNECTION" ]]; then
		echo -n " %(!.%F{red}.%F{blue})%n"
	fi
	# @host
	if [[ -n "$SSH_CONNECTION" ]]; then
		echo -n "%F{green}@%m"
	fi
}

_gravemind_prompt_short_dir_default() {
	echo -n " %F{blue}%30>$GMPSTRUNC1>%1~%<<"
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
			trunc="$GMPSTRUNC/$trunc"
		fi
		trunc="/$trunc"
	fi
	echo -n " %F{blue}$GMPSGIT %20>$GMPSTRUNC1>${b}%<<%F{black}%20>$GMPSTRUNC1>$trunc%<<"
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
	[[ $_GRAVEMIND_GIT -eq 1 ]] || return 1
	local gitdir="$_GRAVEMIND_GIT_GITDIR"
	local toplevel="$_GRAVEMIND_GIT_TOPLEVEL"
	local headref="$_GRAVEMIND_GIT_HEAD_REF"
	local headsha="$_GRAVEMIND_GIT_HEAD_SHA"

	local b t d

	case "$headref" in
		HEAD) b="%F{magenta}${headsha:0:10}"; ;;
		refs/heads/*) b="%F{green}${headref#refs/heads/}"; ;;
		*) b="%F{red}${headref}?"; ;;
	esac
	[[ -z "$b" ]] || b=" %25<$GMPSTRUNC1<$b%<<"

	# local tag
	# tag="$(git -C "$toplevel" describe --tags $headsha 2> /dev/null)"
	# if [[ $? -eq 0 ]]; then
	# 	t="%F{yellow}$tag"
	# fi

	local tag
	# tag="$(git -C "$toplevel" describe --tags --long --dirty 2> /dev/null)"
	tag="$(git -C "$toplevel" describe --tags --long $headsha 2> /dev/null)"
	if [[ $? -eq 0 ]]; then
		case "$tag" in
			*-dirty) d=" %F{red}$GMPSGITDIRTY"; tag="${tag%-dirty}"; ;;
		esac
		case "$tag" in
			# Not exact, but should do
			*-0-g*) t="%F{yellow}${tag%-0-g*}"; ;;
			# *) t="%b%F{yellow}${tag%-*-g*}%B"; ;;
			*) t="%F{black}${tag%-*-g*}"; ;;
		esac
		t="$t"
	fi
	[[ -z "$t" ]] || t=" %25<$GMPSTRUNC1<$t%<<"

	echo -n "$t$b$d"
	return 0
}

gravemind_prompt_cc() {
	local o
	if [[ -n "$CC" ]]; then
		local ccname="$(basename "$CC")"
		o+=" %F{blue}$ccname"
	fi
	if [[ "$ENABLE_RTAGS" == "1" ]]; then
		o+=" %F{blue}R"
	fi
	if [[ "$ENABLE_CCACHE" == "1" ]]; then
		o+=" %F{blue}C"
	fi
	echo -n "$o"
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
	fi
	[[ -z "$o" ]] || echo -n " $o"
}

gravemind_prompt_proxy() {
	local o
	if [[ -n "${http_proxy:-}" || -n "${https_proxy:-}" ]]; then
		o="%F{green}P"
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
	[[ $sec -le 3 ]] || c="%F{blue}"
	echo -n " $c$GMPSDUR1$r$GMPSDUR2"
}

gravemind_prompt_umask() {
	local umask="$(umask)"
	[[ "$umask" -ne "$GRAVEMIND_DEFAULT_UMASK" ]] || return 0
	echo -n " %F{yellow}$umask"
}

gravemind_prompt_group() {
	local group="$(id -gn)"
	[[ "$group" != "$GRAVEMIND_DEFAULT_GROUP" ]] || return 0
	echo -n " %F{yellow}$group"
}

gravemind_prompt_venv() {
	local venv
	: "${venv:="$VIRTUAL_ENV"}"
	: "${venv:="$CONDA_DEFAULT_ENV"}"
	[[ -n "$venv" ]] || return 0
	echo -n " %F{cyan}%20<$GMPSTRUNC1<$venv%<<"
}

gravemind_prompt_spack() {
	local spack
	: "${spack:="$SPACK_ENV"}"
	[[ -n "$spack" ]] || return 0
	echo -n " %F{cyan}%20<$GMPSTRUNC1<$spack%<<"
}

gravemind_build_prompt() {
	echo -n "%K{black}%B"
	if [[ "$_shell_depth" -gt 1 ]]; then
		local i
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
	gravemind_prompt_group
	gravemind_prompt_umask
	gravemind_prompt_spack
	gravemind_prompt_venv
	gravemind_prompt_git
	gravemind_prompt_dir
	gravemind_prompt_cc
	gravemind_prompt_agent
	gravemind_prompt_proxy
	echo -n " %F{black}%D{%H:%M}" # time
	echo -n " %B%F{white}$GMPSEND" # suffix
}

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
