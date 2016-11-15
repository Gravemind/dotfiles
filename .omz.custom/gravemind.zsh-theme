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

RPS1='%K{black}%B%F{white}%{❰%G%} $(gravemind_git_prompt_info_long) %F{white}%{∶%G%} %F{black}!%h %F{blue}%D{%H:%M} %F{white}%{❱%G%}%f%b%k'

PROMPT2='%K{black}%B  %F{blue}%_ %F{white}%{❱%G%} %f%b%k'

unset PROMPT_USER

function gravemind_git_prompt_info_short() {
  local toplevel="$(command git rev-parse --show-toplevel 2>/dev/null)"
  if [[ -z "$toplevel" ]]; then
	  echo "%F{blue}%30>…>%1~%<</"
	  return
  fi
  if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" = "1" ]]; then
	  echo "%F{blue}%30>…>%1~%<</"
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
  echo "%F{blue}⌥ %20>…>$gitrootname%<<%F{black}%20>…>$shortsubgit%<</"
}

function gravemind_git_prompt_info_long() {
  local toplevel="$(command git rev-parse --show-toplevel 2>/dev/null)"
  if [[ -z "$toplevel" ]]; then
	  echo "%F{black}%~"
	  return
  fi
  if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" = "1" ]]; then
	  echo "%F{black}%~ %F{white}%{∶%G%} %F{black}(oh-my-zsh.hide-status)"
	  return
  fi
  ## FIXME when pwd is outside toplevel
  local toplevelbase="$(basename "$toplevel")"
  local toplevelparent="$(dirname "$toplevel")/"
  local pwdbegin="${toplevelparent/#$HOME\//~/}"
  local pwdgit="$toplevelbase"
  local pwdend="${$(pwd)#$toplevel}"
  echo "%F{blue}$(git_current_branch) %F{white}%{∶%G%} %F{black}$pwdbegin%F{blue}$pwdgit%F{black}$pwdend"
}

## refresh prompt every 60 sec
## builtin TRAPALRM called every TMOUT
## http://stackoverflow.com/questions/13125825/zsh-update-prompt-with-current-time-when-a-command-is-started
TMOUT=$(( 5 + 60 - $(date '+%s') % 60 ))
TRAPALRM() {
	zle reset-prompt
	TMOUT=$(( 5 + 60 - $(date '+%s') % 60 ))
}
