#!/bin/bash

## https://gist.github.com/Tadly/0e65d30f279a34c33e9b
## https://gist.github.com/rumpelsepp/d646750910be19332753
## https://gist.github.com/ansemjo/c1761088e9dda47ddd046f6e4ce6aaf4

set -euo pipefail

buildroot="$(mktemp -d)"
mkdir -p "$buildroot"
pushd "$buildroot"

sudo -v

# Install "cower" from AUR
mkdir cower
pushd cower
## Dave Reisner https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=cower
gpg --recv-keys 487EACC08557AD082088DABA1EB2638FF56C0C53
curl -o PKGBUILD https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=cower
makepkg --syncdeps --install --needed --noconfirm PKGBUILD
popd

# Install "pacaur" from AUR
mkdir pacaur
pushd pacaur
curl -o PKGBUILD https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=pacaur
makepkg --syncdeps --install --needed --noconfirm PKGBUILD
popd

# Clean up...
popd
rm -rf "$buildroot"
