#!/bin/zsh


HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='fg=red,bold'

my_sourcing_again="${my_sourcing_again:-false}"
$my_sourcing_again || export _shell_depth="$((${_shell_depth:-0} + 1))"

function _my_starship_precmd(){
	printf "\a" # alert
}
starship_precmd_user_func="_my_starship_precmd"

_STARSHIP_ENV_VAR=""
[[ $_shell_depth -lt 2 ]] || _STARSHIP_ENV_VAR+="$_shell_depth "
# _STARSHIP_ENV_VAR+="bash "
export _STARSHIP_ENV_VAR

eval "$(starship init zsh)"

## refresh prompt every 60 sec
## builtin TRAPALRM called every TMOUT
## http://stackoverflow.com/questions/13125825/zsh-update-prompt-with-current-time-when-a-command-is-started
TMOUT=$(( 5 + 60 - $(date '+%s') % 60 ))
TRAPALRM() {
	zle reset-prompt
	TMOUT=$(( 5 + 60 - $(date '+%s') % 60 ))
}

# # GraveMind PS SEParator/BEGin/END
# GMPSSEP="%F{white}%{∶%G%}"
# GMPSBEG="%{❰%G%}"
# GMPSEND="%{❱%G%}"
# # Git icon
# #GMPSGIT="%{%G%}"
# GMPSGIT="%{⌥%G%}"
# #GMPSGIT="%{%G%}"
# #GMPSGIT="%{%G%}"
# # RPS1="%K{black}%B%F{white}$GMPSBEG\$(gravemind_prompt_cmd_time) \$(gravemind_prompt_info_long) $GMPSSEP \$(gravemind_promt_cc)$GMPSSEP %F{blue}%D{%H:%M} %F{white}$GMPSEND%f%b%k"
# RPS1="%K{black}%B%F{white}$GMPSBEG %F{blue}%D{%H:%M} %F{white}$GMPSEND%f%b%k"

my_sourcing_again=true
