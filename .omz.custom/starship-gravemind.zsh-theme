#!/bin/zsh

HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='fg=red,bold'

_STARSHIP_ENV_VAR=""
export _shell_depth="$((${_shell_depth:-0} + 1))"
[[ $_shell_depth -lt 2 ]] || _STARSHIP_ENV_VAR+="$_shell_depth "
# _STARSHIP_ENV_VAR+="bash "
export _STARSHIP_ENV_VAR

eval "$(starship init zsh)"

# GraveMind PS SEParator/BEGin/END
GMPSSEP="%F{white}%{∶%G%}"
GMPSBEG="%{❰%G%}"
GMPSEND="%{❱%G%}"
# Git icon
#GMPSGIT="%{%G%}"
GMPSGIT="%{⌥%G%}"
#GMPSGIT="%{%G%}"
#GMPSGIT="%{%G%}"

# RPS1="%K{black}%B%F{white}$GMPSBEG\$(gravemind_prompt_cmd_time) \$(gravemind_prompt_info_long) $GMPSSEP \$(gravemind_promt_cc)$GMPSSEP %F{blue}%D{%H:%M} %F{white}$GMPSEND%f%b%k"
RPS1="%K{black}%B%F{white}$GMPSBEG %F{blue}%D{%H:%M} %F{white}$GMPSEND%f%b%k"
