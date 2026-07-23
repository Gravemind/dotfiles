# Personal code snippets


## Bash and Linux

### bash strict template, with helper functions

```sh
#!/bin/bash

set -euo pipefail

log() { echo "$*" >&2; }
die() { log "error: $*"; exit 1; }
run() { local c; c="$(printf ' %q' "$@")"; log "+$c"; "$@" || die "command failed ($?):$c"; }

main() {
    local here
    here="$(cd "$(dirname "$0")"; pwd)"

    run ls
    [[ ... ]] || die "some error"
}

main "$@"
```

- `#!/bin/bash` is probably more portable than `#!/usr/bin/bash`
- `set -u`: error on use of unset variable
- `set -e`: error on uncaught command error
- `set -o pipefail`: error on uncaught command error in a pipeline chain

### array/list operations

```sh
# Command output lines to array
mapfile -t array < <( command )

# Command output words to array
mapfile -t array -d " " < <( command )

# Shift array
shifted=("${array[@]:1}")
```

### log to stdout, but escape pipes and redirections

```sh
#!/bin/bash

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

### Duplicate stdout and stderr to a logfile

```sh
#!/bin/bash

# Merge stdout and stderr to stdout, and duplicate to logfile
exec > >(tee logfile) 2>&1

# with timestamps in file
exec > >(tee >(ts "[%y-%m-%d %H:%M:%.S]" > logfile)) 2>&1
```

### bash tmpdir

```sh
tmpdir="$(mktemp -d -t myscript.XXXXXX)"
# shellcheck disable=SC2064
trap "$(printf 'rm -rf -- %q ||:' "$tmpdir")" EXIT
```

### bash debug helpers

```sh
# Better debug prefix (bash -x, set -x):
PS4=$'\e[1;30m''${BASH_SOURCE[0]}:${LINENO:-}+ '$'\e[0m'
set -x
```

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

### tar

```sh
tar --owner=0 --group=0 --no-same-owner --no-same-permissions -caf tar.tar.xz -C dir file ...
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

### make

Makefile variable assignment:

| assignment     | evaluation     | overridable from |
|----------------|----------------|------------------|
| `VAR = val`    | lazy, each use | cli              |
| `VAR ?= val`   | lazy, each use | env and cli      |
| `VAR := val`   | once at decl   | cli              |
| `VAR != shell` | once at decl   | cli              |

Overridable from cli: `make VAR=42`. Overridable from env: `VAR=42 make`.

make automatic/magic variables:`$@` is the targeted rule name, `$<` is first prerequisite. See https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html


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
