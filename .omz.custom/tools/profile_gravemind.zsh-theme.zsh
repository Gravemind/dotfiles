#!/usr/bin/env zsh

source ~/.zshrc

zmodload zsh/zprof

exec_funcs() {
	for f in "$@" ; do
		"$f"
	done
}

test() {
	echo
	echo "Testing $*"
	"$@" || {
		echo
		echo "$* FAILED"
		exit 1
	}
	echo
}

profile() {
	echo
	echo "Profiling $*:"
	zprof -c
	repeat 100 "$@" > /dev/null
	zprof
}

test gravemind_preexec
test gravemind_precmd
# test exec_funcs "${preexec_functions[@]}"
# test exec_funcs "${precmd_functions[@]}"
test gravemind_build_prompt
test gravemind_build_rprompt

fakeprompt() {
	gravemind_preexec
	gravemind_precmd
	# exec_funcs "${preexec_functions[@]}"
	# exec_funcs "${precmd_functions[@]}"
	gravemind_build_prompt
	gravemind_build_rprompt
}

zprof -c
# profile gravemind_build_rprompt
profile fakeprompt
zprof
