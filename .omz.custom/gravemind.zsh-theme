#!/bin/zsh

HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='fg=red,bold'

PROMPT_USER=""
if [[ $EUID -ne 1000 || -n "$SSH_CONNECTION" ]]; then
	PROMPT_USER="%(!.%F{red}.%F{blue})%n"
fi
if [[ -n "$SSH_CONNECTION" ]]; then
	PROMPT_USER="${PROMPT_USER}%F{green}@$HOST"
fi
if [[ -n "$PROMPT_USER" ]]; then
	PROMPT_USER=" $PROMPT_USER %F{white}%{∶%G%}"
fi

## http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html

PROMPT='%K{black}%B%F{white}%{❰%G%}'"${PROMPT_USER}"' $(gravemind_git_prompt_info_short)%1(j. %F{white}%{∶%G%} %F{green}%j.)%(?.. %F{white}%{∶%G%} %F{red}%?) %F{white}%{❱%G%} %f%b%K{black}'

RPS1='%K{black}%B%F{white}%{❰%G%} $(gravemind_git_prompt_info_long) %F{white}%{∶%G%} $(gravemind_promt_cc) %F{white}%{∶%G%} %F{blue}%D{%H:%M} %F{white}%{❱%G%}%f%b%k'

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

function gravemind_git_prompt_current_branch() {
    if $GRAVEMIND_PROMPT_USE_GITFAST
    then
        __git_ps1 "%s"
    else
        git_current_branch
    fi
}

function gravemind_git_prompt_info_short() {
  local toplevel="$(command git rev-parse --show-toplevel 2>/dev/null)"
  if [[ -z "$toplevel" ]]; then
	  echo -n "%F{blue}%30>…>%1~%<</"
	  return
  fi
  if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" = "1" ]]; then
	  echo -n "%F{blue}%30>…>%1~%<</"
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
  echo -n "%F{blue}⌥ %20>…>$gitrootname%<<%F{black}%20>…>$shortsubgit%<</"
}

function gravemind_git_prompt_info_long() {
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
	local str=""
	if [[ "$ENABLE_RTAGS" == "1" ]]; then
	   str+='%F{blue}R'
	else
	   str+='%F{black}R'
	fi
	if [[ "$ENABLE_CCACHE" == "1" ]]; then
	   str+='%F{blue}C'
	else
	   str+='%F{black}C'
	fi
	local cc="${CC:-cc}"
	local ccname="$(basename "$cc")"
	echo -n "%F{black}$ccname $str"
}

function gravemind_alert_precmd() {
	## .Xresource: URxvt*urgentOnBell: true
	echo -ne '\a'
}
## prepend
precmd_functions=(gravemind_alert_precmd "${precmd_functions[@]}")

## refresh prompt every 60 sec
## builtin TRAPALRM called every TMOUT
## http://stackoverflow.com/questions/13125825/zsh-update-prompt-with-current-time-when-a-command-is-started
TMOUT=$(( 5 + 60 - $(date '+%s') % 60 ))
TRAPALRM() {
	zle reset-prompt
	TMOUT=$(( 5 + 60 - $(date '+%s') % 60 ))
}
