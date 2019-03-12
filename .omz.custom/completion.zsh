
zstyle ':completion:*' show-ambiguity "1;$color[fg-white]"

# https://github.com/robbyrussell/oh-my-zsh/issues/1398#issuecomment-255581289
#zstyle ':completion:*' matcher-list 'r:|=*' '+ r:|[._-]=* l:|=*'

# sort file completion by modified date
#   https://stackoverflow.com/questions/50761901/make-zsh-tab-autocomplete-cycle-by-modification-date
#zstyle ':completion:*:cd:*' file-sort modification
zstyle ':completion:*' file-sort modification
#zstyle ':completion:*:(ls|cat|vim):*' file-sort modification
