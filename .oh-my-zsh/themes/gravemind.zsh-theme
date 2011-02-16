
PROMPT='%{$fg_bold[blue]%}|%(?..%{$fg[red]%} %?%{$reset_color%}) %{$fg_bold[black]%}%j %{$fg_bold[black]%}%m%{$fg_bold[blue]%} :: %{$fg_bold[black]%}%~
%{$fg_bold[blue]%}| %{$reset_color%}%{$bold_color%}%T %{$fg_bold[blue]%}%n %# %{$reset_color%}'

PROMPT2='%{$fg_bold[blue]%}| %{$fg_bold[black]%}%_ %{$fg_bold[blue]%}> %{$reset_color%}'

RPS1='$(git_prompt_info)'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[black]%}[%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg_bold[black]%}]%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="*"
ZSH_THEME_GIT_PROMPT_CLEAN=""
