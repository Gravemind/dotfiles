
function gge() {
	\emacs --eval "(progn (jo/prepare-gdb) (gdb \"gdb -i=mi $* \"))" &
}

function ggv() {
	gdb -ex 'target remote | /usr/lib/valgrind/../../bin/vgdb' "$@"
}

compdef _gdb ggv=gdb


alias ggg='gdb --args'
