#!/bin/zsh

CR="%{$reset_color%}"
CB="%{$fg_bold[black]%}"
CF="%{$fg_bold[blue]%}"
CW="%{$bold_color%}"
COK="%{$fg_bold[green]%}"
CKO="%{$fg_bold[red]%}"

PROMPT_SSH="${CB}"
[ -n "$SSH_CONNECTION" ] && PROMPT_SSH="${COK}"

function prompt_color_pwd() {
    PROMPT_PWDA="$(pwd)"
    PROMPT_PWD="${PROMPT_PWDA#$HOME}"
    [ "$PROMPT_PWDA" != "$PROMPT_PWD" ] && PROMPT_PWD="~$PROMPT_PWD"
    if [ ${#PROMPT_PWD} -lt 2 ]
    then
        echo "$fg_bold[blue]$PROMPT_PWD"
    else
        echo "${PROMPT_PWD%/*}/$fg_bold[blue]${PROMPT_PWD##*/}"
    fi
}

PROMPT='${CF}| %(?..${CKO}%? )%1(j.${COK}%j .)${PROMPT_SSH}%m${CF}:${CB}$(prompt_color_pwd)${CR}
${CF}| ${CF}%n %(!.${CKO}\#.${CB}\$)${CR} '

PROMPT2='{CF}| ${CB}%_ ${CF}>${CR} '

RPS1='$(git_prompt_info) ${CW}(%T)${CR}'

ZSH_THEME_GIT_PROMPT_PREFIX="${CB}[${CF}"
ZSH_THEME_GIT_PROMPT_SUFFIX="${CB}]${CR}"
ZSH_THEME_GIT_PROMPT_DIRTY="${CKO}*"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_UNTRACKED="${CKO}?"
