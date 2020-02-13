
function gge() {
	\emacs --eval "(progn (jo/prepare-gdb) (gdb \"gdb -i=mi --args $* \"))" &
}

function gge-rust() {
	\emacs --eval "(progn (jo/prepare-gdb) (gdb \"rust-gdb -i=mi --args $* \"))" &
}

function ggv() {
	gdb -ex 'target remote | /usr/lib/valgrind/../../bin/vgdb' "$@"
}

compdef _gdb ggv=gdb


alias ggg='gdb --args'
