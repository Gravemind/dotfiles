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

```sh
# Hard-format, zero first 40MiB
dd bs=4M count=10 conv=sync,fsync status=progress if=/dev/zero of=/dev/disk/by-id/usb-...

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
strace -rttT  # relative/absolute/syscall times
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
| CONT      | 18     | 🔒Continue  | Continue if stopped. (handler allowed)                                   |
| STOP      | 19     | 🔒Stop      | Stop (pause) the process.                                                |
| TSTP      | 20     | Stop        | Stop (pause) typed at terminal.                                          |

🔒: Cannot be caught, blocked, or ignored. SIGCONT can have a handler.

```sh
# graceful kill: TERM then KILL after 2s
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
