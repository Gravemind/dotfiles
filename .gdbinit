
set print pretty on
set print object on
set print static-members off
set print array on
set print array-indexes on
set print vtbl on
set print demangle on
set print asm-demangle on
set demangle-style gnu-v3

set print inferior-events on

set confirm off
#set pagination off

set non-stop off
set disassembly-flavor intel

set history save on
set history size 2048
set history remove-duplicates 1

set python print-stack full

handle SIGPWR nostop
handle SIGXCPU nostop

source ~/.gdbinit.local
