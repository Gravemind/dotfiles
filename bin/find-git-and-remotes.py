#!/usr/bin/env python3
#
# find-git-and-remotes <haystack> <maxdepth>
# find-git-and-remotes /path 10
#

import os
import subprocess as subp
import sys
from concurrent import futures
from pathlib import Path


def find_git_repos(path, maxdepth):
    try:
        it = path.iterdir()
    except OSError as e:
        print(f"warning: {e}: {path}", file=sys.stderr)
        return
    pend = []
    for e in it:
        if e.name == ".git":
            yield path
            # Don't recurse inside repos
            return
        elif maxdepth > 0 and e.is_dir():
            pend.append(e)
    for e in pend:
        yield from find_git_repos(e, maxdepth - 1)


def git_remotes(path):
    urls = []
    with subp.Popen(
        ["git", "-C", str(path), "remote", "-v"],
        encoding="utf8",
        stdout=subp.PIPE,
    ) as proc:
        out, _err = proc.communicate()
        for line in out.splitlines():
            spl = line.split(maxsplit=2)
            if len(spl) > 2:
                _name, url, *_rest = spl
                urls.append(url)
    return urls


def only_completed(fs):
    fs = set(fs)
    return (f for f in fs if f.done())


def main():
    haystack = Path(sys.argv[1])
    maxdepth = int(sys.argv[2])

    rolling_pad = None

    def completed(path, remotes):
        path = str(path.relative_to(haystack))
        remotes = sorted(set(remotes))
        l = len(path)
        mod = 16
        padl = ((l // mod) + 1) * mod
        nonlocal rolling_pad
        if rolling_pad is None:
            rolling_pad = max(padl, 30)
        rate = 0.2 if padl > rolling_pad else 0.1
        rolling_pad += (padl - rolling_pad) * rate * len(remotes)
        pad = (((max(l, int(rolling_pad)) - 1) // mod) + 1) * mod
        for remote in remotes:
            print(f"{path:<{pad}} {remote}")

    # for path in find_git_repos(haystack, maxdepth):
    #     completed(path, git_remotes(path))

    with futures.ThreadPoolExecutor(max_workers=os.process_cpu_count()) as executor:
        f2p = {}
        repos_it = find_git_repos(haystack, maxdepth)
        for path in repos_it:
            f2p[executor.submit(git_remotes, path)] = path
            for f in only_completed(f2p):
                completed(f2p.pop(f), f.result())
        for f in futures.as_completed(f2p):
            completed(f2p[f], f.result())


if __name__ == "__main__":
    main()
