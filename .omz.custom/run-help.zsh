
unalias run-help
autoload -U run-help

autoload run-help-git

_run-help-sub-command() {
	local msg="$*"
	local cmd="$1"
	shift
	local mansubcmd=
	local mancmd_fallback="$cmd"
	while [[ $# -gt 0 ]]
	do
		case "$1"
		in
			-*) ;;
			*)
			mancmd_fallback=
			if man -w "$cmd-$1"
			then
				mansubcmd="$cmd-$1"
				break
			fi
			;;
		esac
		shift
	done
	if [[ -n "$mansubcmd" ]]
	then
		echo "running 'man $mansubcmd' found in $msg"
		man "$mansubcmd"
	elif [[ -n "$mancmd_fallback" ]]
	then
		echo "running 'man $cmd'. no apparent subcommand in $msg"
		man "$cmd"
	else
		echo "error: could not find a valid 'man $cmd-<subcommand>' in $msg"
	fi
}

# default run-help-sudo does not recurse run-help (sudo git add)
# autoload run-help-sudo
run-help-sudo() {
	if [ $# -eq 0 ]; then
		man sudo
	else
		run-help "$@"
	fi
}

# /usr/share/zsh/functions/Misc/run-help-git
run-help-docker() {
	if [ $# -eq 0 ]; then
		man docker
	elif [[ $# -gt 1 && "$2" == [a-z]* ]]; then
		man docker-$1-$2
	else
		# local al
		# if al=$(git config --get "alias.$1"); then
		#	1=${al%% *}
		# fi
		man docker-$1
	fi
}

run-help-flatpak() {
	echo "run-help $@" >&2
	if [ $# -eq 0 ]; then
		man flatpak
	else
		_run-help-sub-command flatpak "$@"
	fi
}

run-help-perf() {
	if [ $# -eq 0 ]; then
		man perf
	else
		man perf-$1
	fi
}

run-help-semanage() {
	if [ $# -eq 0 ]; then
		man semanage
	else
		man semanage-$1
	fi
}

run-help-openssl() {
	if [ $# -eq 0 ]; then
		man openssl
	else
		man openssl-$1
	fi
}
