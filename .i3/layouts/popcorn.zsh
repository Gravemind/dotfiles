#!/bin/zsh

HERE="$(cd "`dirname "$0"`"; pwd)"

## do not eval $HH_SDK_ROOT now:
PK='$HH_SDK_ROOT'

PK_BIN="$PK/release/builds/x64_Samples"
PK_GMAKE="$PK/source_tree/projects/gmake_linux"

$HERE/urxvt.zsh 1 "PK_BIN" "cd \"$PK_BIN\"; clear"

$HERE/urxvt.zsh 3 "PK_ROOT" "cd \"$PK\" ; clear"

i3-msg "workspace 9; append_layout $HERE/pidgin.json"
pidgin & ; disown

i3-msg "workspace 2; append_layout $HERE/pk_ws2.json;"
urxvt -name PK_EMACS -e zsh -is eval "cd \"$PK_GMAKE\"; rtagson; emacs .  -e 'make-frame' & ; cd \"$PK\";" & ; disown

i3-msg "workspace 10; append_layout $HERE/firefox.json"
firefox & ; disown

$HERE/rtags.zsh

i3-msg "workspace 1"
i3-msg "workspace 10"
