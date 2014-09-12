#!/bin/zsh

# alias ne='~/bin/ne -q -l ~/.emacs.d/config/jo-config.el'
# alias nw='\emacs -q -l ~/.emacs.d/config/jo-config.el -nw'
# alias ns='emacscl ns'
alias nw='emacs -nw'

ne() {
    emacs "$@" &
}

ec() {
    emacsclient -c "$@" &
}

ed() {
    emacs --daemon
}

ke() {
    pkill -9 emacs
}

##
## emacs kill deamon
##
## usage: ek DAEMON_NAME
##
ek() {
    DAEMON_NAME="$1"
    if [[ -z $DAEMON_NAME ]]
    then
	    echo "usage: $0 emacs_deamon_name_to_kill"
	    exit 1
    fi
    PID=`ps ax | grep -- "--daemon=$DAEMON_NAME" | grep -v 'grep'  | sed -r 's/\s*([0-9]+).*/\1/g'`
    if [[ -z $PID ]]
    then
	    echo 'no daemon found'
	    exit 0
    fi
    kill -9 $PID
}

##
## emacs daemon service
##
## usage: emacsserver [start|stop|restart] DAEMON_NAME
##
emacsserver() {
    DAEMON_NAME="$2"
    case "$1" in
	    start)
		    ${0} stop $DAEMON_NAME
		    emacs "--daemon=$DAEMON_NAME" &> /dev/null
		    ;;
	    restart)
		    ${0} start $DAEMON_NAME
		    ;;
	    stop)
		    PID=`ps ax | grep -- "--daemon=$DAEMON_NAME" | grep -v 'grep' | cut -d ' ' -f 1`
		    echo "$PID"
		    if [[ -n "$PID" ]]
		    then
			    kill $PID
		    fi
		    ;;
	    *)
		    echo "usage: $0 {start|stop|restart}"
    esac
}

##
## emacs client auto launch daemon
##
## usage: emacscl DEAMON_NAME [args...]
##
emacscl() {
    DAEMON_NAME="$1"
    shift
    PID=`ps ax | grep -- "--daemon=$DAEMON_NAME"| grep -v 'grep'  | sed -r 's/\s*([0-9]+).*/\1/g'`
    echo "$PID"
    if [[ -z "$PID" ]]
    then
	    emacsserver start $DAEMON_NAME
    fi
    emacsclient -s $DAEMON_NAME -c -n "$@"
}
