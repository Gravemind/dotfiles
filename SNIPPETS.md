# Personal code snippets


## Bash

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
trap "$(printf 'rm -rf %q ||:' "$tmpdir")" EXIT
```

### Prefix all output lines with date

```sh
command | ts '[%y-%m-%d %H:%M:%.S]'
```

### Date

```sh
date +%y%m%d-%H%M%S
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

