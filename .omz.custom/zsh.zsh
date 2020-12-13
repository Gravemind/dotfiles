#!/bin/zsh

export HISTSIZE=100000
export SAVEHIST=100000

## man zshoptions
## http://zsh.sourceforge.net/Doc/Release/Options.html

# Always append history, do not replace it
setopt append_history

# Enable only ONE of the following:
# - (default) Command history is read on startup, and saved at exit
: nop
# - Command history is read on startup, and saved before all commands
setopt inc_append_history
# - Command history is read on startup, and saved after all commands (with command duration)
unsetopt inc_append_history_time
# - Command history is re-read and saved before all commands
unsetopt share_history

# Unset option that tries to correct spelling errors
unsetopt correct_all

# Unset option that nices background jobs
unsetopt bg_nice

#unsetopt extended_glob
