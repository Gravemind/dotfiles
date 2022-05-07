
sudo pacman -S base-devel linux-tools man-pages sudo zsh bc man-db emacs vim git ripgrep time

sudo pacman -S ufw
sudo ufw enable

cd
git clone --bare https://github.com/Gravemind/dotfiles.git .dotfiles.git
ln -sfT -r .dotfiles.gitignore .dotfiles.git/info/exclude
ln -s .dotfiles.git .git
git config core.bare false
git config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'
git fetch
git reset HEAD .
git status
git checkout .

git clone https://aur.archlinux.org/yay.git
cd yay && makepkg -si
yay -S yay-bin

## System Adm

yay -S --asexplicit --needed \
    openssh rsync sysstat ldns bind-tools ntp socat \
    iftop iotop atop powertop \
    lm_sensors smartmontools hdparm acpi \
    dosfstools ntfs-3g nfs-utils parted \
    nvme-cli hwinfo \
    dkms linux-headers

yay -S collectd

## User

yay -S --asexplicit --needed \
    zsh-completions bash-completion \
    ripgrep fd moreutils autojump readline bc wget pwgen gnu-netcat neofetch tree tldr pv \
    hyperfine \
    imagemagick gnuplot \
    p7zip unzip pigz zstd \
    pacutils reflector pacman-contrib expac pyalpm pkgfile expac \
    vulkan-devel

yay -S --asdeps --needed cni-plugins
yay -S --asexplicit --needed podman buildah

## Dev Tools, Languages

yay -S --asexplicit --needed \
    ispell aspell aspell-en aspell-fr \
    cmake ninja meson \
    gdb valgrind strace ccache boost cloc \
    jq asciidoc dos2unix astyle \
    clang clang-tools-extra llvm \
    ruby python dmd go rustup rust-racer \
    python-i3-py python-matplotlib python-pyserial python-pyqt5

yay -S --asexplicit --needed \
    android-file-transfer android-tools android-udev

yay -S avrdude dfu-programmer arduino-cli

## Desktop Environment (Xorg, WM, Tools)

yay -S --asexplicit --needed \
    xorg xterm xlogin-git \
    xkb-qwerty-fr \
    i3 dmenu rofi ttf-dejavu ttf-hack ttf-nerd-fonts-symbols-mono ttf-font-awesome \
    rxvt-unicode urxvt-perls \
    dunst xdg-user-dirs tk feh \
    mesa-demos \
    xdotool xclip clipmenu clipnotify \
    firefox chromium \
    numlockx xautolock xss-lock xbindkeys \
    redshift \
    playerctl

yay -S --asexplicit --needed \
    meld qpdfview qalculate-gtk gnumeric xdiskusage gimp gpick audacity \
    mpv youtube-dl libva-vdpau-driver \
    steam steam-fonts

pulseaudio pulseaudio-alsa pamixer pavucontrol
flatpak
flameshot : screenshot

(for winetricks)
yay -S --asexplicit --needed cabextract

yay -S --asexplicit --needed virtualbox virtualbox-host-dkms virtualbox-ext-oracle

## nvidia

yay -S --needed nvidia opencl-nvidia nvidia-dkms nvidia-settings

<!-- (Enable multilib in pacman.conf) -->
<!-- yay -S lib32-nvidia-utils -->
