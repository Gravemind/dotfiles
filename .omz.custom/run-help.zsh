
unalias run-help
autoload -U run-help

autoload run-help-git

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
		# 	1=${al%% *}
		# fi
		man docker-$1
	fi
}
