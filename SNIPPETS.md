# Personal code snippets


## Bash and Linux

### bash strict template, with helper functions

```sh
#!/bin/bash

set -euo pipefail

log() { echo "$*" >&2; }
fatal() { log "error: $*"; exit 1; }
run() { local c; c="$(printf ' %q' "$@")"; log "+$c"; "$@" || fatal "command failed ($?):$c"; }

main() {
    local here
    here="$(cd "$(dirname "$0")"; pwd)"
}

main "$@"
```

- `#!/bin/bash` is probably more portable than `#!/usr/bin/bash`
- `set -u`: error on use of unset variable
- `set -e`: error on uncaught command error
- `set -o pipefail`: error on uncaught command error in a pipeline chain

### bash tmpdir

```sh
tmpdir="$(mktemp -d -t myscript.XXXXXX)"
# shellcheck disable=SC2064
trap "$(printf 'rm -rf -- %q ||:' "$tmpdir")" EXIT
```

### Prefix all output lines with date

```sh
command | ts '[%y-%m-%d %H:%M:%.S]'
```

### Date

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

### tar

```sh
tar --owner=0 --group=0 --no-same-owner --no-same-permissions -caf tar.tar.xz -C dir file ...
```

## Python

### Python2 and Pytnon3 unicode

```py
# -*- coding: utf-8 -*-
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
