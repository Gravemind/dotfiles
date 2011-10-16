#!/bin/zsh

CR="%{$reset_color%}"
CBG="%{$bg[black]%}"
CB="%{$fg_bold[black]%}"
CW="%{$bold_color%}"
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
    PROMPT_SSH="${COK}@"
else
    PROMPT_SSH="${CB}@"
fi

PROMPT='${CBG}${CF}❰ ${CF}%n${PROMPT_SSH}%m%1(j. ${COK}%j.)%(?.. ${CKO}%?) ${CF}%(!.❱❱.❱) ${CR}%E'

RPS1='%{$bg[black]%}${CB}%~$(git_prompt_info)${CR}'

PROMPT2='%{$bg[black]%}${CF}❰ ${CB}%_ ${CF}❱ ${CR}'

ZSH_THEME_GIT_PROMPT_PREFIX="${CF}:"
ZSH_THEME_GIT_PROMPT_SUFFIX="${CR}"
ZSH_THEME_GIT_PROMPT_DIRTY="${CKO}✕"
ZSH_THEME_GIT_PROMPT_CLEAN="${COK}✓"
ZSH_THEME_GIT_PROMPT_UNTRACKED="${CKO}?"
