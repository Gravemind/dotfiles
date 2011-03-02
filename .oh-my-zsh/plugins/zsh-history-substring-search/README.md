zsh-history-substring-search
============================

**[Fish shell](http://www.fishshell.org) like history search for [Zsh](http://www.zsh.org).**


## Try it

Here is a one-liner to try it without installing or modifying anything:

    wget --no-check-certificate --output-document=/tmp/zsh-history-substring-search.plugin.zsh https://github.com/sunaku/zsh-history-substring-search/raw/master/zsh-history-substring-search.plugin.zsh && . /tmp/zsh-history-substring-search.plugin.zsh


## Install it


### In your ~/.zshrc

* Download the script or clone this repository:

      git clone git://github.com/sunaku/zsh-history-substring-search.git

* Source the script **at the end** of `~/.zshrc`:

      source /path/to/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh

* Source `~/.zshrc`  to take changes into account:

      source ~/.zshrc


### With oh-my-zsh

* Download the script or clone this repository in [oh-my-zsh](http://github.com/robbyrussell/oh-my-zsh) plugins directory:

      cd ~/.oh-my-zsh/plugins/
      git clone git://github.com/sunaku/zsh-history-substring-search.git

* Activate the plugin in `~/.zshrc` (in **last** position):

      plugins=( [plugins...] zsh-history-substring-search)

* Source `~/.zshrc`  to take changes into account:

      source ~/.zshrc


## Authors / Greetings

* [Peter Stephenson](http://www.zsh.org/mla/users/2009/msg00818.html)
* [Guido van Steen](https://github.com/steentje)
* [Suraj N. Kurapati](https://github.com/sunaku)
