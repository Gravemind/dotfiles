
if [ "$TERM" = "rxvt-unicode-256color" ]
then
    export TERM="rxvt-256color"
fi

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

export EDITOR='emacs'
export PAGER='less'
export LESS='-R'
export PATH="$PATH:/home/jo/bin"
export OSTYPE=`uname -s`
export BROWSER='/usr/bin/chromium-browser'

export CNORM_PATH='/home/jo/lib/cnorm_2.10'
export KOOC_PATH='/home/jo/documents/epitech/KOOC'
export ATISTREAMSDKROOT='/home/jo/lib/ati-stream-sdk-v2.3-lnx64'
export ATISTREAMSDKSAMPLESROOT="$ATISTREAMSDKROOT/samples"
export LD_LIBRARY_PATH="$ATISTREAMSDKROOT/lib/x86_64:$LD_LIBRARY_PATH"

export PACMAN='pacman-color'
export WINEARCH="win32"
export WINEPREFIX="$HOME/win32"
