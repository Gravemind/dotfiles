
## Bare git dotfiles:

Tweaked version of the dotfiles as git bare repo way:
* https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/
* https://news.ycombinator.com/item?id=11071754

Here is a short How To:

#### Setup: Bare clone to `.dotfiles.git`, add the `.git` symlink:

```bash
cd ~
git clone --bare https://github.com/Gravemind/dotfiles .dotfiles.git

# Enable "HOME as git repo" (`rm .git` to disable)
ln -s .dotfiles.git .git

# Unbare
git config --local core.bare false
git config --local remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'
git fetch

# All files will be marked as staged for deletion,
# so we unstage "deleted" files
git reset HEAD .
```

#### Use: Enable/Disable HOME as git repo:

- Disable by removing the symlink `rm ~/.git`
- Enable by creating the symlink `cd ~ ; ln -sfT .dotfiles.git .git`

See [helpers in `.omz.custom/dotfiles.zsh`](../.omz.custom/dotfiles.zsh)

When enabled, HOME behaves like any other git repo:

```bash
cd ~
git add .mydotfile1
git checkout .mydotfile2
git diff ...
# gitk/magit/UIs/etc...
```

#### Tips: Ignore all except ..., using `.git/info/exclude`:

Create and commit a **[`.dotfiles.gitignore`](../.dotfiles.gitignore)** file, for example:

```bash
# ignore all:
/*
# except:
!/.dotfiles.gitignore
!/.gitmodules
!/.mydotfile1
!/bin
```

Or `git checkout .dotfiles.gitignore` if already commit.

Setup it as `.git/info/exclude` (should conflict less than a `~/.gitignore`, see `man gitignore`):

```bash
cd ~
ln -sfT -r .dotfiles.gitignore .dotfiles.git/info/exclude
```

#### Tips: You can use submodules

Add/update submodules like in other git repo.

Setup/Init existing submodules (those already commit):

```sh
git checkout .gitmodules
# .. rm -rf any existing dirs ...
git submodule update --init
```

#### Tips: Config for other machines as branches, rebased on main config
