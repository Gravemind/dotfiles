
function gge() {
	\emacs --eval "(progn (jo/enable-linum) (gdb \"gdb -i=mi $1\"))" &
}

function ggv() {
	gdb -ex 'target remote | /usr/lib/valgrind/../../bin/vgdb' "$@"
}

compdef _gdb ggv=gdb


alias ggg='gdb --args'
