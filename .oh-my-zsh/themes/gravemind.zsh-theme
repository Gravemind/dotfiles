#!/bin/zsh

CR="%{$reset_color%}"
CB="%{$fg_bold[black]%}"
CF="%{$fg_bold[blue]%}"
CW="%{$bold_color%}"
COK="%{$fg_bold[green]%}"
CKO="%{$fg_bold[red]%}"

HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='fg=red,bold'

PROMPT_SSH=" "
[ -n "$SSH_CONNECTION" ] && PROMPT_SSH="${COK}@"

function prompt_color_pwd() {
    PROMPT_PWDA="$(pwd)"
    PROMPT_PWDA="${(q)PROMPT_PWDA}"
    PROMPT_PWD="${PROMPT_PWDA#$HOME}"
    PROMPT_PWD="${(q)PROMPT_PWD}"
    [ "$PROMPT_PWDA" != "$PROMPT_PWD" ] && PROMPT_PWD="~$PROMPT_PWD"
    if [ ${#PROMPT_PWD} -lt 2 ]
    then
        # echo"${CF}$PROMPT_PWD"
    else
        # echo "${CB}${PROMPT_PWD%/*}/${CF}${PROMPT_PWD##*/}"
    fi
}

# PROMPT='${CF}| %(?..${CKO}%? )%1(j.${COK}%j .)${PROMPT_SSH}%m${CF}:${CB}$(prompt_color_pwd)${CR}
# ${CF}| ${CF}%n %(!.${CKO}#.${CB}\$)${CR} '
#RPS1='$(git_prompt_info) ${CW}(%T)${CR}'

PROMPT='%{$bg[black]%}${CF}| ${CB}%m${PROMPT_SSH}${CF}%n %1(j.${COK}%j .)%(?..${CKO}%? )%(!.${CKO}#.${CB}\$) ${CR}%E'
#RPS1='%{$bg[black]%}$(prompt_color_pwd)${CR} $(git_prompt_info) ${CR} '

RPS1='%{$bg[black]%}${CB}%~$(git_prompt_info)${CR}'

PROMPT2='%{$bg[black]%}${CF}| ${CB}%_ ${CF}> ${CR}'

ZSH_THEME_GIT_PROMPT_PREFIX="${CF}:"
ZSH_THEME_GIT_PROMPT_SUFFIX="${CR}"
ZSH_THEME_GIT_PROMPT_DIRTY="${CKO}*"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_UNTRACKED="${CKO}?"
