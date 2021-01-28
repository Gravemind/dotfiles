#!/bin/zsh

export HISTSIZE=100000
export SAVEHIST=100000

## man zshoptions
## http://zsh.sourceforge.net/Doc/Release/Options.html

# Always append history, do not replace it
setopt append_history

# Choose only ONE of the following option:
# - <none-set>: Command history is read on startup, and saved at exit
# - set inc_append_history: Command history is read on startup, and saved before all commands
# - set inc_append_history_time: ditto, with command duration
# - set share_history: Command history is re-read and saved before all commands
setopt inc_append_history
unsetopt inc_append_history_time
unsetopt share_history

# Unset option that tries to correct spelling errors
unsetopt correct_all

# Unset option that nices background jobs
unsetopt bg_nice

#unsetopt extended_glob
