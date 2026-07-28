
# Who TF aliases run-help to man?
unalias run-help 2>/dev/null ||:

autoload run-help
# autoload run-help-git # See ours
autoload run-help-ip
autoload run-help-openssl
autoload run-help-p4
# autoload run-help-sudo  # See ours
autoload run-help-svk
autoload run-help-svn

# Resolve git aliases
run-help-git() {
	local sub="${1:-git}"

	git help "$sub" || {
		return 1
	}

	local resolved
	resolved="$(git config --get "alias.$sub" 2>/dev/null || true)"

	if [[ -n "$resolved" ]]
	then
		local -a cmd=( "${(z)resolved}" )

		# Note: zsh uses one-based indexing
		if [[ "${cmd[1]}" = "!git" ]]
		then
			shift cmd
		elif [[ "${cmd[1]}" = "!"* ]]
		then
			echo "(no further help for unknown git alias script)"
			return 1
		fi

		while [[ "${#cmd[@]}" -gt 0 && "${cmd[1]}" = -* ]]
		do
			shift cmd
		done

		# printf -- '-%s-\n' "${cmd[@]}"
		local res="${cmd[1]}"
		if [[ -n "$res" ]]
		then
			# See /usr/share/zsh/functions/Misc/run-help
			local what newline='
'
			builtin print -nP "%SPress any key for more help on 'git $res' or q to quit%s"
			builtin read -k what
			[[ $what != $newline ]] && echo
			[[ $what == [qQ] ]] && break

			run-help-git "${cmd[@]}"
		else
			echo "(no further help for unknown git alias command)"
			return 1
		fi
	fi
}

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

_run-help-prepend--help() {
	local base="$1"
	shift

	# run-help strips options/paths/etc from command line,
	# so get back original command line:
	local cmd_args
	builtin getln cmd_args &&
	builtin print -z "$cmd_args"
	cmd_args=( ${(z)${cmd_args:-"$*"}} )
	# FIXME: discard stuff ? see run_help_orig_cmd in /usr/share/zsh/functions/Misc/run-help

	set -- "${cmd_args[@]}"

	if [[ "${1:-}" = *"$base" ]] ; then
		local a0="$1"
		shift
		local helpcmd=("$a0" --help "$@")
		printf "(_run-help-prepend--help:" >&2
		printf -- " %s" "${helpcmd[@]}" >&2
		printf ")\n" >&2
		"${helpcmd[@]}"
	fi
}

run-help-nix-env() { _run-help-prepend--help nix-env "$@"; }
run-help-nix-store() { _run-help-prepend--help nix-store "$@"; }
