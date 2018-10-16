
## Bare git dotfiles:

Tweaked version of the dotfiles as a git bare repo way:
* https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/
* https://news.ycombinator.com/item?id=11071754

Here is a short How To:

#### Setup: Clone --no-checkout --separate-git-dir:

```bash
cd ~
git clone --no-checkout --separate-git-dir=.dotfiles.git https://github.com/Gravemind/dotfiles .
# Clone staged all files as deleted, un-stage that:
git reset HEAD .
```

#### Use: HOME as a git repo:

Note: the `git clone` already globally made your HOME a git repo: it created a
`~/.git` file referencing `~/.dotfiles.git`.

* To globally disable HOME as a git repo: `rm ~/.git`
* To globally enable: `cd ~ ; ln -sfT .dotfiles.git .git`
* [See helpers in `.omz.custom/dotfiles.zsh`](../.omz.custom/dotfiles.zsh), eg:
  * `dotfiles` globally toggles HOME as git repo (with symlink `.git` to `.dotfiles.git`)
  * `dot` alias to `git --git-dir=...`, so you can `dot status/add/commit` etc...

When globally enabled, HOME behaves like any other git repo:

```bash
cd ~
git add .mydotfile1
git checkout .mydotfile2
git diff ...
# gitk/magit/UIs/etc...
```


#### Gitignore: Ignore all except ..., using `.git/info/exclude`:

Create and commit a **[`.dotfiles.gitignore`](../.dotfiles.gitignore)** file, for example:
```bash
# ignore all:
/*
# except:
!/.dotfiles.gitignore
!/.gitmodules
!/.mydotfile1
!/bin
#!/...
```

Or `git checkout .dotfiles.gitignore` if already commit.

Setup it as `.git/info/exclude` (should conflict less than a `~/.gitignore`, see `man gitignore`):
```bash
cd ~
ln -sfT -r .dotfiles.gitignore .dotfiles.git/info/exclude
```

#### Submodules: Checkout, update, and init:

Add/update submodules like in other git repo.

Setup/Init existing submodules (those already commit):
```sh
git checkout .gitmodules
# .. rm -rf any existing dirs ...
git submodule update --init
```

#### Multi-configs: as branches rebased onto the master config
