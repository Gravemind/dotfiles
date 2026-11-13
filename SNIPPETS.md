# Personal code snippets


## Bash and Linux

### bash strict template, with helper functions

```bash
#!/bin/bash

set -euo pipefail

log() { echo "$*" >&2; }
die() { log "error: $*"; exit 1; }
run() { local c; c="$(printf ' %q' "$@")"; log "+$c"; "$@" || die "command failed ($?):$c"; }

main() {
    [[ -e does/this/exists ]] || die "does not exist, will exit"
    run cmd fatal on failure
    local r
    cmd not fatal || { r=$?; log "just a warning ($r)"; return $r; }
}

main "$@"
```

- `#!/bin/bash` is probably more portable than `#!/usr/bin/bash`

- `set -u`: error on use of unset variable

- `set -e`: error on uncaught command failure, except in `&&`, `||`, `if`, `while`, `!`, etc.

  > [!WARNING]
  > Cannot replace explicit command failure check.

- `set -o pipefail`: error on uncaught command error in a pipeline chain

- `run cmd...`: run cmd with a message, with fatal error (exit) on failure.


### more bash misc tricks

```bash
$@  # args (excluding $0)
$#  # length of $@ (so, not counting $0)
```

```bash
bash -c 'echo args: $@' -- args...
```

```bash
local here
here="$(cd "$(dirname "$0")"; pwd)"
```

```bash
export PS4='+ '
set -x
```

```bash
# https://flokoe.github.io/bash-hackers-wiki/scripting/debuggingtips/
export PS4='+ $(printf "[%-30s] " "$$ ${BASH_SOURCE:-?}:${LINENO:-?} ${FUNCNAME[0]:-}${FUNCNAME[0]:+()}")'
set -x
```

```bash
shopt -s nullglob  # Non-matching glob returns nothing
```

```bash
# Run command with message, exit on failure
run() { logrun "$@" || exit $?; }
# Run command with message, just return exit code
logrun() {
    local c="$(printf ' %q' "$@")"
    log "+$c"
    "$@" || {
        local r=$?
        log "command failed ($?):$c"
        return $r
    }
}
```

A "logging" `set -e`, still doesn't replace proper explicit command failure checks:

```bash
set -o errexit   # Exit on uncaught command error, won't catch if/while/&&/||/!/etc.
set -o errtrace  # Inherit ERR trap in functions
aterror() {
    local ret="$1" cmd="$2" file="$3" line="$4" func="$5"
    echo "(trap ERR) $file: line $line${func:+: in }${func}${func:+()}: command exited $ret: $cmd" >&2
    exit "$ret"  # Equivalent to set -e
}
trap 'aterror "$?" "${BASH_COMMAND:-?}" "${BASH_SOURCE[0]:-?}" "${LINENO:-?}" "${FUNCNAME[0]:-}"' ERR
```

```sh
tmpdir="$(mktemp -d -t myscript.XXXXXX)"
# shellcheck disable=SC2064
trap "$(printf 'rm -rf -- %q ||:' "$tmpdir")" EXIT
```

### array/list operations

```bash
# Command output lines to array
mapfile -t array < <( command )

# Command output words to array
mapfile -t array -d " " < <( command )

# Shift array
shifted=("${array[@]:1}")
```

### log to stdout, even inside pipes and redirections

```sh
#!/bin/bash

shopt -s varredir_close  # Close {fd} at exit

# Duplicate stdout fd for logging (makes it immune to future pipes and redirs)
exec {loggingfd}>&1
log() { echo "$*" >&$loggingfd; }

# example:
{
    echo foobar    # piped to sed
    log debug      # not piped, still prints to stdout
    echo err >&2   # not piped, prints to stderr (as expected)
} | sed 's/^/piped:/'
```

### Merge stderr in stdout, and redirect a copy to a log file

```sh
#!/bin/bash

# Merge stderr in stdout, and redirect a copy to a log file
exec > >(tee logfile) 2>&1

# with timestamps in file
exec > >(tee >(ts "[%y-%m-%d %H:%M:%.S]" > logfile)) 2>&1
```

AFAIK merge is required to get proper stdout/stderr sync in the log file.

### Prefix all output lines with date

```sh
command | ts '[%y-%m-%d %H:%M:%.S]'
```

### Date

from timestamp:
```sh
date -d @1758809457
```

simple:
```sh
date +%y%m%d-%H%M%S
```

iso 8601:
```
date -u +%Y%m%dT%H%M%SZ      # UTC, short
date -u +%Y-%m-%dT%H:%M:%SZ  # UTC, long
date +%Y%m%dT%H%M%S%z        # NON-UTC, short
date +%Y-%m-%dT%H:%M:%S%:z   # NON-UTC, long
```

### xdg dir

```sh
set -euo pipefail

xdg_dir() {
    local value
    value="$(eval echo "\${XDG_${1}_DIR:-}")"
    [[ -z "$value" ]] || { echo "$value"; return 0; }
    value="$(
        # See /usr/bin/xdg-user-dir
        # shellcheck source=/dev/null
        test -f "${XDG_CONFIG_HOME:-~/.config}/user-dirs.dirs" && . "${XDG_CONFIG_HOME:-~/.config}/user-dirs.dirs"
        eval echo "\${XDG_${1}_DIR:-}"
    )"
    [[ -z "$value" ]] || { echo "$value"; return 0; }
    echo "$2"
}

XDG_DOWNLOAD_DIR="$(xdg_dir DOWNLOAD "$HOME/Downloads")"
XDG_RUNTIME_DIR="$(xdg_dir RUNTIME "/run/user/$UID")"
XDG_CONFIG_DIR="$(xdg_dir CONFIG "$HOME/.config")"

declare | grep '^XDG_'
```

### dd

- `dd bs=4M count=512`: transfer 512 * 4 * 1024 * 1024 bytes total
- `dd conv=sync`: pad input with zeros if not multiple of bs
- `dd conv=fsync`: long flush at the end
- `dd oflag=sync`: flush each block: more accurate progress and bandwidth, no wait at the end

```sh
# Format partition table
# dd bs=512 count=1 conv=sync,fsync oflag=sync status=progress if=/dev/zero of=/dev/disk/by-id/usb-...
wipefs -a /dev/disk/by-id/usb-...

partprobe

# Create partiion
fdisk /dev/disk/by-id/usb-...
# type 0c "0c W95 FAT32 (LBA)" for classic USB FAT32

partprobe

# Create FAT32 FS
mkfs.vfat -F 32 -n PARTLABEL /dev/disk/by-id/usb-...-part1
```

```sh
# Burn iso on usb
dd bs=4M conv=sync,fsync status=progress if=...iso of=/dev/disk/by-id/usb-...
```

### gawk

```
# Match regex and capture groups
gawk 'match($0, /pat(ter)n/, m) { ter = m[1]; }'
```

### perl oneliners

```sh
man perlrun
```

```sh
# Perl substitute but with perl code eval

# Substitute all matches in text and print all new text
perl -pe 's/[0-9]+/ sprintf("%04d", $& + 42) /eg'

# Print only matches, substituted
perl -ne 's/^ foo (.*?) bar (.*?)$/ print $1."\/".$2."\n"; /e'
```

```sh
# Readable regex code and commants: /x ignores non-backslashed whitespaces, and allow '#' comments.
perl -pe 's/
  # This is a comment, not part of regex.
  # The following is part of the regex match:
  [a-z0-9]+
  # Whitespaces and newlines match nothing, and must be matched explicitly
  \ + \s+ [ \t]+
/repl/x'
```

```sh
# Matching over multi-line input
# * -0777 : slurps the whole file (777 is a convention)
# * //m   : /^/ and /$/ match begin and end of each lines in input, instead of begin and end of whole input
# * //s   : /./ also match newlines (note: /\s/ always match newlines, /\s+$/ match trailing spaces)
perl -0777 -pe 's///gms'
```

### tar + zstd

```sh
tar --owner=0 --group=0 --no-same-owner --no-same-permissions -caf tar.tar.zst -C parentdir dir-or-file ...

tar -I "zstd -T0 --fast=1 --long" -cf /tmp/test.tar.zst ...
tar -cf - ... | zstd -T0 --fast=1 --long > /tmp/test.tar.zst
```

| ratio  | comp seed | decomp sepeed | cmd                       | note                                         |
|--------|-----------|---------------|---------------------------|----------------------------------------------|
| high   | mid       | mid           | zstd -T0 -3 --long        | high ratio. ~= faster xz.                    |
| mid    | fast      | fast          | zstd -T0 --fast=1 --long  | faster decomp. ~= faster zlib.               |
| low    | fast      | fast          | zstd -T0 --fast=3         | lower memory usage. ~= less hungry "lz4 -1". |
| low    | fast      | fastest       | zstd -T0 --fast=22 --long | fastest decomp                               |
| lowest | fastest   | fast          | zstd -T0 --fast=22        | fastest comp                                 |

- zstd -#       : higher # = higher ratio, slower decomp, much slower comp.
- zstd --fast=# : higher # = lower ratio, faster decomp, barely faster comp (when -T6).
- zstd --long   : x10 memory usage (10MB -> 100MB), better ratio and faster decompression, but slower comp.

```sh
# Transfer with adaptive compression:
# - For a small transfer on slow speed (e.g. <=10GB file on <=1Gbit eth):
tar -cf - -C parentdir dir | zstd -T0 --long --fast=22 --adapt -v | ssh remote 'zstd -d | tar -xf - -C parentdir'
# - For large transfers, or on high speed, multithread adapt will get stuck at high level very low speed, so:
tar -cf - -C parentdir dir | zstd -T1 --long --fast=22 --adapt -v | ssh remote 'zstd -d | tar -xf - -C parentdir'
# - But a fixed level, multithreaded, deduced by benchmark, will be the fastest.
```

### Slurm

```sh
# QoS
sacctmgr show qos | less
```

```sh
# Reservations
sacctmgr show reservation
```

### lsof

```sh
lsof /path

lsof -p pid,pid

# List open file for all "zsh" processes
lsof -c zsh

# List network connections
lsof -i
```

### ssh

```sh
# List ssh key fingerprint (md5)
find ~/.ssh -name '*.pub' -exec ssh-keygen -E md5 -lf {} \;

# Test ssh authentification to github.com/gitlab.com
ssh -T git@github.com
ssh -T -i ~/.ssh/id_ed25519 -p 22 git@github.com

# Generate ssh key
ssh-keygen -t ed25519 -C "your_email@example.com" -f ~/.ssh/my_ssh_key
```

### curl

```sh
curl -sSLf -O <url>  # Better default

curl -sSLf -C - -R --connect-timeout 60 --max-time 600 --retry 3 -o <file> <url>  # Advanced. Add --compressed if content is not already compressed

curl -sS       # Silent, but show errors
curl -L        # Follow redirections
curl -f        # Exit error on HTTP errors
curl -O        # Output to a file named from URL
curl -o <file> # Output to <file>

curl -R            # Try to set output file modification date to the remote file modif time
curl -C -          # Resume/Continue transfer
curl --compressed  # Ask for compression. Note: curl decompresses but leaves compression header in response

curl --connect-timeout 60  # Connection timeout
curl --max-time 600        # Transfert timeout
curl --retry 3             # Retry. Reset timeouts on retry.
```


### stat

```
stat -c '%s %n' /file
stat --help | grep perm
```

### git

```
# don't clone blobs (alternative to shallow clone)
git clone --filter=blob:limit=200k ...
```

### strace

```
strace -f -rttT -o /tmp/trace cmd...
strace -f     # follow fork
strace -rttT  # relative+absolute+syscall times
```

### signals

| SIG       | number | action      | description                                                              |
|-----------|--------|-------------|--------------------------------------------------------------------------|
| HUP       | 1      | Terminate   | Hangup detected on controlling terminal or death of controlling process. |
| INT       | 2      | Terminate   | Interrupt from keyboard (Ctrl+C)                                         |
| QUIT      | 3      | Coredump    | Quit from keyboard (Ctrl+\\)                                             |
| ILL       | 4      | Coredump    | Illegal instruction                                                      |
| ABRT, IOT | 6      | 🔒Coredump  | Abort signal from abort(3). IOT trap.                                    |
| BUS       | 7      | Coredump    | Bus error (bad memory access)                                            |
| FPE       | 8      | Terminate   | Erroneous arithmetic operation (e.g., divide by zero).                   |
| KILL      | 9      | 🔒Terminate | Kill signal.                                                             |
| USR1      | 10     | Terminate   | User-defined signal 1.                                                   |
| SEGV      | 11     | Coredump    | Segmentation fault; invalid memory reference.                            |
| USR2      | 12     | Terminate   | User-defined signal 2.                                                   |
| PIPE      | 13     | Terminate   | Broken pipe: write to pipe with no readers pipe(7).                      |
| ALRM      | 14     | Terminate   | Timer signal from alarm(2).                                              |
| TERM      | 15     | Terminate   | Termination signal.                                                      |
| CHLD, CLD | 17     | Ignore      | Child stopped, terminated, or continued.                                 |
| CONT      | 18     | 🔒*Continue | Continue if stopped. (handler allowed)                                   |
| STOP      | 19     | 🔒Stop      | Stop (pause) the process.                                                |
| TSTP      | 20     | Stop        | Stop (pause) typed at terminal.                                          |

🔒: Cannot be caught, blocked, or ignored.
🔒*: Ditto except SIGCONT can have a handler.

### kill

```sh
# graceful kill: send TERM then KILL after 2s
/bin/kill --verbose --timeout 2000 KILL --signal TERM "$pid" 2>/dev/null
```

### errno

| number | hex  | symbol      | description                                             |
|--------|------|-------------|---------------------------------------------------------|
| 1      | 0x01 | EPERM       | Operation not permitted                                 |
| 2      | 0x02 | ENOENT      | No such file or directory                               |
| 3      | 0x03 | ESRCH       | No such process                                         |
| 4      | 0x04 | EINTR       | Interrupted system call                                 |
| 5      | 0x05 | EIO         | Input/output error                                      |
| 6      | 0x06 | ENXIO       | No such device or address                               |
| 7      | 0x07 | E2BIG       | Argument list too long                                  |
| 8      | 0x08 | ENOEXEC     | Exec format error                                       |
| 9      | 0x09 | EBADF       | Bad file descriptor                                     |
| 10     | 0x0a | ECHILD      | No child processes                                      |
| 11     | 0x0b | EAGAIN      | Resource temporarily unavailable                        |
| 11     | 0x0b | EWOULDBLOCK | (Same value as EAGAIN) Resource temporarily unavailable |
| 12     | 0x0c | ENOMEM      | Cannot allocate memory                                  |
| 13     | 0x0d | EACCES      | Permission denied                                       |
| 14     | 0x0e | EFAULT      | Bad address                                             |
| 15     | 0x0f | ENOTBLK     | Block device required                                   |
| 16     | 0x10 | EBUSY       | Device or resource busy                                 |
| 17     | 0x11 | EEXIST      | File exists                                             |
| 18     | 0x12 | EXDEV       | Invalid cross-device link                               |
| 19     | 0x13 | ENODEV      | No such device                                          |
| 20     | 0x14 | ENOTDIR     | Not a directory                                         |
| 21     | 0x15 | EISDIR      | Is a directory                                          |
| 22     | 0x16 | EINVAL      | Invalid argument                                        |

### permissions chmod

| symbolic       | octal                         |
|----------------|-------------------------------|
| rwx            | 04 + 02 + 01 = 07             |
| u=rwx,g=rw,o=x | 0700 + 0060 + 0001 = 0761     |
| u+s,g+s,a+t    | 04000 + 02000 + 01000 = 07000 |

- `u+s`: "set-user-ID-on-execution", set uid bit, SUID bit.
- `g+s`: "set-group-ID-on-execution", set gid bit, SGID bit.
- `a+t` or `+t`: "sticky bit", SVTX bit. On directories, it prevents rename or delete entries not
  owned, for example: set on world-writable /tmp.

### acl

```sh
$ getfacl /path
$ setfacl -m user:buddy:rw /path
```

| acl           | def                          |
|---------------|------------------------------|
| `user::rwx`   | owning user                  |
| `group::rwx`  | owning group                 |
| `user:U:rwx`  | some user U                  |
| `group:G:rwx` | some group G                 |
| `mask::rwx`   | masks owning group, U, and G |
| `other::rwx`  | others                       |

`mask::`:
- masks `group::`, `user:U:`, and `group:G:`
- does **NOT mask `user::` nor `other::`**
- replaces the "unix group" in classic file permissions:
  - read/write group with `ls`/`chmod`/etc. actually reads/writes the `mask::`

```sh
$ ls -ld /path
-rwxrw-r--+ 1 owner group ... /path

          + = with acl,
 rwx        = user::rwx           = owner
    rw-     = mask::rw- (NOT grp) = masks group::, group:G:, and user:U:
       r--  = other::r--          = others

$ chmod g-w /path   # changes the acl mask (!)
```

### makefile

Makefile variable assignment:

| assignment     | evaluated                               | overridable from (*) |
|----------------|-----------------------------------------|----------------------|
| `VAR = val`    | at each rule start for each use in rule | cli                  |
| `VAR ?= val`   | at each rule start for each use in rule | env and cli          |
| `VAR := val`   | once at decl                            | cli                  |
| `VAR != shell` | once at decl                            | cli                  |

(*): Overridable from cli: `make VAR=42`. Overridable from env: `VAR=42 make`.

Rules:

```make
# Rule
target1 target2: first-prereq other-prereq | order-only-prereq
     echo $@ : the running target
     echo $< : the first prereq
     echo $^ : all prereqs
     echo $? : outdated prereqs

# Rule with pattern
obj/%.o: src/%.c
     echo $* : the matching '%' in % patterns

# Rule with pattern, only for some files
$(filter %.o,$(files)): obj/%.o: src/%.c
```

Magic variables, see https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html .

Some functions:

```make
dirname := $(dir path)
filename := $(notdir path)
abspath := $(abspath names...)
resolved-canonical-realpath := $(realpath names...)
glob := $(wildcard *.o)
substitute-match := $(patsubst %.c,%.o,$(files))
```

Make each rules execute in a single bash shell instance, add bash strict flags:

```make
SHELL = bash
.SHELLFLAGS = -eu -c # Catch command errors and unbound variables
# .SHELLFLAGS = -eu -x -o pipefail -c  # + trace + pipefail
.ONESHELL:

.PHONY: a
a:
	@a=42   # @ just means silent
	echo a=$$a
```

### gdb

```
info proc           # pid, cmd, exe, cwd
info program        # current state, exception, signal

info symbol ADDR    # the function/section/binary/lib/exe of an address
info address SYM    # the address of a symbol/function

p arr@3             # print 3 elements of array (pointer) "arr"

set env ENV VAL     # set environment variable
show env ENV VAL    # get environment variable

thread appy all bt  # backtrace of all threads

info signals
handle <SIGS...> <ACTIONS...>
handle SIGUSR1 print nostop pass  # Print signal, and pass to inferior process
handle SIGUSR1 print stop pass    # Print signal, stop, and pass to inferior process

```


### perf

```
sudo sysctl kernel.perf_event_paranoid=1  # >=2:per-process user, >=1:+kernel, >=0:+system-wide, >=-1:+tracepoints ftrace
```

### magic sysrq REISUB

https://en.wikipedia.org/wiki/Magic_SysRq_key
https://wiki.archlinux.org/title/Keyboard_shortcuts
https://www.kernel.org/doc/html/latest/admin-guide/sysrq.html#what-are-the-command-keys

Linux kernel commands keyboard shortcuts. Must be enabled:

```console
$ cat /proc/sys/kernel/sysrq
244
$ cat /etc/sysctl.d/99-sysrq.conf
# Magic sysrq
# 1: allow everything
# 244: allow reisub (no log level, no dump, no nice)
kernel.sysrq=244
```

Keep Alt pressed then press/release in sequence: PrintScr (SysRq), R, E, I, S, U, B, (reisub: Raising Elephants Is So Utterly Boring)

| Cmd | Function                                                                    |
|-----|-----------------------------------------------------------------------------|
| r   | Turns off keyboard raw mode and sets it to XLATE.                           |
| e   | SIGTERM processes, except init.                                             |
| i   | SIGKILL processes, except init.                                             |
| s   | Sync mounted filesystems.                                                   |
| u   | Read-only remount mounted filesystems as  .                                 |
| b   | Reboot without syncing or unmounting your disks.                            |
|     |                                                                             |
| h   | Help (works with any unknown key)                                           |
| f   | OOM killer                                                                  |
|     |                                                                             |
| c   | Performs a system crash and a crashdump will be taken if configured.        |
| d   | Shows all locks that are held.                                              |
| g   | Used by kgdb (kernel debugger)                                              |
| j   | Forcibly "Just thaw it" - filesystems frozen by the FIFREEZE ioctl.         |
| k   | Kills all programs on the current virtual console. NOTE: See important doc. |
| l   | Shows a stack backtrace for all active CPUs.                                |
| m   | Dump current memory info to your console.                                   |
| n   | Used to make RT tasks nice-able                                             |
| o   | Shutdown.                                                                   |
| p   | Dump current registers and flags.                                           |
| q   | Dump per CPU lists of all armed hrtimers, clockevent devices, etc..         |
| t   | Dump tasks.                                                                 |
| v   | Forcefully restores framebuffer console                                     |
| v   | Causes ETM buffer dump [ARM-specific]                                       |
| w   | Dumps tasks that are in uninterruptible (blocked) state.                    |
| z   | Dump the ftrace buffer                                                      |
| 0-9 | Sets the console kernel log level (0=panic, 3=err, 4=warn, 7=info).         |
| R   | Replay the kernel log messages on consoles.                                 |


### podman (rootless)

(This is for rootless podman. Rootful behavior can differ: --userns etc. see man.)

```sh
# Run inside current directory (rw), at same pwd, with same UID
podman run --rm -it --userns=keep-id -v "${PWD}:${PWD}:rw" -w "${PWD}" image /bin/sh

# Ditto, but pwd modifications stay inside the container (leave host $PWD untouched)
podman run --rm -it --userns=keep-id -v "${PWD}:${PWD}:rw" -w "${PWD}" image /bin/sh
```

| Volume option                            |                                                               |
|------------------------------------------|---------------------------------------------------------------|
| --volume volumename:/cont                | mount a named volume (podman volume list)                     |
| --volume /host:/cont                     | mount a host directory                                        |
|                                          |                                                               |
| --volume /host:/cont:z                   | shared selinux label                                          |
| --volume /host:/cont:Z                   | private selinux label                                         |
| --volume /host:/cont:U                   | on startup, chown -R UID:GID /host (UID = host uid or subuid) |
| --volume /host:/cont:noexec,nosuid,nodev | block usage within container (default is nosuid,nodev)        |
| --volume /host:/cont:O                   | put an overlay fs (leave host untouched)                      |

| Userns option UID/GID mapping       | cont -> host | cont -> host         | Subuid range used                 |
|-------------------------------------|--------------|----------------------|-----------------------------------|
| --userns=auto                       | 0 -> S+off   | X -> S+off           | S+off to S+off+1024 (non-overlap) |
| --userns=nomap                      | 0 -> S       | X -> S               | all subuids (overlaps!)           |
| --userns=host or --userns="" (def.) | 0 -> $UID    | X -> S-1             | all subuids (overlaps!)           |
| --userns=keep-id                    | 0 -> S       | $UID -> $UID, X -> S | all subuids (overlaps!)           |
| --userns=keep-id:uid=200            | 0 -> S       | 200 -> $UID, X -> S  | all subuids (overlaps!)           |

S = First subuid of /etc/subuid for this user
off = offset to get a subuid range not already used (ie not overlapping other containers) from /etc/subuid for this user

This means that --userns=auto containers cannot be run at the same time as a all-subuids-consuming
(the default) containers.

This also means that multiple --userns=host (the default) containers with the same UID number inside
the container, also maps to the same host UID outside. So they can share some permissions (in volumes
etc.), this is can be both useful and a security consideration.

### podman compose (rootless)

`.env` file used in `compose.yaml` variable expansions. defaults of `podman-compose --env-file .env`.

`services:X:env_file: .other.env` is used for container environment.


## Python

### Python2 and Python3 unicode

```py
# -*- coding: utf-8 -*-
```


### IPython/Jupyter

```py
%xmode Verbose  # Exception handlers: Plain, Context, Verbose, and Minimal
%debug          # After an exception, run this to open post-mortem ipdb
%pdb on         # Automatic post-mortem pdb debug on exception
```

## Compilers

### Dump compiler preprocessor definitions

```sh
gcc -dM -E -x c /dev/null
g++ -dM -E -x c++ /dev/null
```

### Diff/Dump compiler target options and optimizations

```sh
diff <(gcc -Q --help=target) <(gcc -Q -march=native --help=target)
diff <(gcc -Q -O0 --help=optimizers) <(gcc -Q -O3 --help=optimizers)
```


# Misc

### unicode

https://en.wikipedia.org/wiki/Box-drawing_characters
https://en.wikipedia.org/wiki/Dingbat
https://en.wikipedia.org/wiki/List_of_emojis

```txt
┌─┬┐  ╔═╦╗  ╓─╥╖  ╒═╤╕
│ ││  ║ ║║  ║ ║║  │ ││
├─┼┤  ╠═╬╣  ╟─╫╢  ╞═╪╡
└─┴┘  ╚═╩╝  ╙─╨╜  ╘═╧╛
┬─ ✓ ✗
├─ ‣ ☐ ☑ ☒
└─ • ❎ ✅
```

### font-awesome

```txt
checkboxes:    
```

### PostgreSQL

```
$ pgsql -U user
=# \l        # List databases
=# \c db     # Connect to db
=# \dt       # List tables
=# \d table  # List table columns
=# select * from table where column = 42 ;
```

### ffmpeg

```
ffin=/input/file.X
ffout=/output/file.mkv

# Info
ffprobe "$ffin"

# Info json format
ffprobe -v quiet -print_format json -show_format -show_streams "$ffin"

# Convert to 30fps 720p h264 - software decode and re-encode
ffmpeg -i "$ffin" -vf 'fps=30,scale=1280:-1' -c:v libx264 -preset fast "$ffout"

# https://trac.ffmpeg.org/wiki/Hardware/VAAPI

# Convert to 30fps 720p h264
# - vaapi encode output
ffmpeg -vaapi_device /dev/dri/renderD128 -i "$ffin" -vf 'fps=30,format=nv12,hwupload,scale_vaapi=1280:-1' -c:v h264_vaapi -q 0 -compression_level 1 "$ffout"

# Convert to 30fps 720p h264
# - vaapi decode input (very slow ??)
# - vaapi encode output
ffmpeg -vaapi_device /dev/dri/renderD128 -hwaccel vaapi -hwaccel_output_format vaapi -i "$ffin" -vf 'fps=30,scale_vaapi=1280:-1' -c:v h264_vaapi -q 0 -compression_level 1 "$ffout"

```
