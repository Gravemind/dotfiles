#!/bin/zsh

CR="%{$reset_color%}"
CBG="%{$bg[black]%}"
CB="%{$fg_bold[black]%}"
CW="%{$fg_bold[white]%}"
COK="%{$fg_bold[green]%}"
CKO="%{$fg_bold[red]%}"

if [ $EUID -eq 0 ]; then
    CF="%{$fg_bold[red]%}"    
else
    CF="%{$fg_bold[blue]%}"
fi

HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='fg=red,bold'

if [ -n "$SSH_CONNECTION" ]; then
    PROMPT_SSH="${COK}@$HOST"
else
    PROMPT_SSH=""
fi

PROMPT='${CBG}${CW}❰ ${CF}%n${PROMPT_SSH}%1(j. ${COK}%j.)%(?.. ${CKO}%?) ${CW}❱ ${CR}'

RPS1='${CBG}${CW}❰${CB}%~$(git_prompt_info) ${CF}%D{%H:%M}${CW}❱${CR}'

PROMPT2='${CBG}  ${CF}%_ ${CW}❱ ${CR}'

ZSH_THEME_GIT_PROMPT_PREFIX="${CF}:"
ZSH_THEME_GIT_PROMPT_SUFFIX="${CR}"
ZSH_THEME_GIT_PROMPT_DIRTY="${CKO}✕"
ZSH_THEME_GIT_PROMPT_CLEAN="${COK}✓"
ZSH_THEME_GIT_PROMPT_UNTRACKED="${CKO}?"
