
```bash
sed -rn '/## PACMAN ##/,/## AUR ##/   bk;d;:k;s/^- ([[:alnum:]_-]+).*/\1/gp' base-packages.md | xargs pacman -S --needed

# AUR, with Yay
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si

sed -rn '/## AUR ##/,/## OTHER ##/ bk;d;:k;s/^- ([[:alnum:]_-]+).*/\1/gp' base-packages.md | xargs yay -S --needed
```

# ## PACMAN ## Packages

- base-devel

## System Adm

- openssh
- ufw

- rsync
- sysstat
- linux-tools : (group) perf, etc..
- ldns        : drill
- bind-tools  : dig
- #net-tools  : (deprecated ?)
- ntp

- iftop
- iotop
- atop
- powertop

- lm_sensors
- smartmontools
- hdparm
- acpi

- dosfstools : fat
- ntfs-3g    : ntfs
- nfs-utils
- parted

- socat

## User

- zsh
- zsh-completions
- bash-completion
- git

- ripgrep     : very fast grep
- fd          : very fast find
- moreutils   : pee, sponge, etc...
- autojump
- readline
- bc
- wget
- pwgen
- gnu-netcat
- neofetch
- #mlocate    : locate (replaced by fd on ssd: fast enough)
- tree
- tldr
- pv

- imagemagick
- gnuplot

- p7zip
- pigz
- zstd

- pacutils
- reflector
- pacman-contrib
- expac
- pyalpm
- pkgfile
- expac

- #at         : one-time cron

## Dev Tools, Languages

- emacs
- ispell
- aspell-en
- aspell-fr

- clang
- clang-tools-extra
- cmake
- llvm

- ruby
- python
- dlang
- dlang-ldc
- go
- rustup
- rust-racer

- gdb
- jq
- asciidoc
- dos2unix

- python-i3-py
- python-matplotlib
- python-pyserial
- python-pyqt5

## Desktop Environment (Xorg, WM, Tools)

- xorg
- i3
- dmenu
- dunst
- xdg-user-dirs
- tk
- feh

- pulseaudio
- pamixer
- pavucontrol

- xterm
- rxvt-unicode
- urxvt-perls

- rofi

- ttf-dejavu
- ttf-hack
- otf-font-awesome

- flatpak

- clipmenu
- clipnotify

- numlockx
- xautolock
- xclip
- xdotool
- redshift
- playerctl

- xorg-xprop
- xorg-xev
- xorg-xkill
- xorg-xwininfo

## Graphical Softwares

- firefox
- chromium
- #spotify

- meld
- qpdfview
- qalculate-gtk
- gnumeric
- xdiskusage
- gimp
- gpick

- mpv
- youtube-dl
- libva-vdpau-driver

- flameshot : screenshot

- steam

# ## AUR ## Packages

- xlogin-git
- xkb-qwerty-fr
- steam-fonts

# ## OTHER ## Packages

- rtags: `cd ~/bin ; git clone git@github.com:Andersbakken/rtags`
