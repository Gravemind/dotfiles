#!/bin/zsh

CR='%{$reset_color%}'
CB='%{$fg_bold[black]%}'
CF='%{$fg_bold[blue]%}'
CW='%{$bold_color%}'
COK='%{$fg_bold[green]%}'
CKO='%{$fg_bold[red]%}'

PROMPT_SSH="${CB}"
[ -n "$SSH_CONNECTION" ] && PROMPT_SSH="${COK}"

PROMPT="${CF}| %(?..${CKO}%? )%1(j.${COK}%j .)${CF}:${CB}%~${CR}
${CF}| ${PROMPT_SSH}%m ${CF}%n %(!.${CKO}.${CB})%#${CR} "

PROMPT2="${CF}| ${CB}%_ ${CF}>${CR} "

RPS1="$(git_prompt_info)${CW}(%T)${CR}"

ZSH_THEME_GIT_PROMPT_PREFIX="${CB}[${CF}"
ZSH_THEME_GIT_PROMPT_SUFFIX="${CB}]${CR} "
ZSH_THEME_GIT_PROMPT_DIRTY="${CKO}*"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_UNTRACKED="${CKO}?"
