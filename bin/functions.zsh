#!/bin/zsh


STAT_COL=80
if [[ ! -t 1 ]]; then
	USECOLOR=""
elif [[ -t 0 ]]; then
	# stty will fail when stdin isn't a terminal
	STAT_COL="$(/bin/stty size)"
	# stty gives "rows cols"; strip the rows number, we just want columns
	STAT_COL="${STAT_COL##* }"
elif /bin/tput cols &>/dev/null; then
	# is /usr/share/terminfo already mounted, and TERM recognized?
	STAT_COL=$(/bin/tput cols)
fi
if ((STAT_COL==0)); then
	# if output was 0 (serial console), set default width to 80
	STAT_COL=80
	USECOLOR=""
fi

STAT_COL=$(($STAT_COL - 13))

C_MAIN="\033[1;37m"      # main text

C_OTHER="\033[1;34m"     # prefix & brackets
C_SEPARATOR="\033[1;30m" # separator

C_BUSY="\033[0;36m"      # busy
C_FAIL="\033[1;31m"      # failed
C_DONE="\033[1;37m"      # completed
C_BKGD="\033[1;35m"      # backgrounded

C_H1="\033[1;37m"        # highlight text 1
C_H2="\033[1;36m"        # highlight text 2

C_CLEAR="\033[1;0m"

SAVE_POSITION="\033[s"
RESTORE_POSITION="\033[u"
DEL_TEXT="\033[$(($STAT_COL+4))G"

PREFIX_REG="::"
PREFIX_HL=" >"

function deltext() {
	printf "${DEL_TEXT}"
}

function printhl() {
	printf "${C_OTHER}${PREFIX_HL} ${C_H1}${1}${C_CLEAR} \n"
}

function printsep() {
	printf "\n${C_SEPARATOR}   ------------------------------\n"
}

function stat_bkgd() {
	printf "${C_OTHER}${PREFIX_REG} ${C_MAIN}${1}${C_CLEAR} "
	deltext
	printf "   ${C_OTHER}[${C_BKGD}BKGD${C_OTHER}]${C_CLEAR} "
}

function stat_busy() {
	printf "${C_OTHER}${PREFIX_REG} ${C_MAIN}${1}${C_CLEAR} "
	printf "${SAVE_POSITION}"
	deltext
	printf "   ${C_OTHER}[${C_BUSY}BUSY${C_OTHER}]${C_CLEAR} "
}

function stat_append() {
	printf "${RESTORE_POSITION}"
	printf -- "${C_MAIN}${1}${C_CLEAR}"
	printf "${SAVE_POSITION}"
}

function stat_done() {
	deltext
	printf "   ${C_OTHER}[${C_DONE}DONE${C_OTHER}]${C_CLEAR} \n"
}

function stat_fail() {
	deltext
	printf "   ${C_OTHER}[${C_FAIL}FAIL${C_OTHER}]${C_CLEAR} \n"
}

function stat_die() {
	stat_fail
	exit ${1:-1}
}

function status() {
	stat_busy "$1"
	shift
	if "$@" >/dev/null 2>&1; then
		stat_done
		return 0
	fi
	stat_fail
	return 1
}
