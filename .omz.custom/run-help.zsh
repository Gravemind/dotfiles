
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
