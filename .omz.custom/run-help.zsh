
# Who TF aliases run-help to man?
unalias run-help 2>/dev/null ||:

autoload run-help
autoload run-help-git
autoload run-help-ip
autoload run-help-openssl
autoload run-help-p4
# autoload run-help-sudo  # See ours
autoload run-help-svk
autoload run-help-svn

# default run-help-sudo does not recurse run-help (sudo git add)
run-help-sudo() {
	if [ $# -eq 0 ]; then
		man sudo
	else
		run-help "$@"
	fi
}

# Find man for up to "command subcommand subsubcommand"
_run-help-sub-command() {
	local base="$1"
	shift
	# TODO: support "command -ignore-me subcommand" ?
	if [[ $# -ge 2 && "$1" =~ ^[a-z_-]+$ && "$2" =~ ^[a-z_-]+$ ]] && man -w $base-$1-$2; then
		man $base-$1-$2
	elif [[ $# -ge 1 && "$1" =~ ^[a-z_-]+$ ]] && man -w $base-$1; then
		man $base-$1
	else
		man $base
	fi
}

run-help-docker() {
	_run-help-sub-command docker "$@"
}

run-help-podman() {
	_run-help-sub-command podman "$@"
}

run-help-flatpak() {
	_run-help-sub-command flatpak "$@"
}

run-help-perf() {
	_run-help-sub-command perf "$@"
}

run-help-semanage() {
	_run-help-sub-command semanage "$@"
}
