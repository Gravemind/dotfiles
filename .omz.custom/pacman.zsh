#!/bin/zsh

alias pacman='CC=cc CXX=c++ CPP=c++ pacman'
alias pacaur='CC=cc CXX=c++ CPP=c++ pacaur'
alias pac='CC=cc CXX=c++ CPP=c++ pacaur'

if ! command which pacaur > /dev/null
then
	echo "${fg_bold[red]}pacaur not found${reset_color}"
fi

pacmirrorfile=/etc/pacman.d/mirrorlist
pacmirrorfile_expire_sec=$((4 * 24 * 60 * 60)) # 4 days

# Update mirror list if necessary
pacupmir() {
	local ago=$(( $(date +%s) - $(date +%s -r "$pacmirrorfile") ))
	#local ago=10000
	if [[ "$1" = "-f" || $ago -gt $pacmirrorfile_expire_sec ]]
	then
		echo "$0: updating... (${fg_bold[green]}$(durationtostr $ago old)${reset_color})"
		sudo pacupdatemirrors || { echo "${fg_bold[red]}$0: pacupdatemirrors failed !!$reset_color"; return 1 }
	else
		echo "$0: up to date (${fg_bold[cyan]}$(durationtostr $ago old)${reset_color})"
	fi
}

# Fetch only new updates
pacup() {
	pacupmir || { echo "$0: pacupmir failed"; return 1; }
	echo "$0: fetching updates..."
	pacaur -Syuw --noconfirm --noedit || { echo "${fg_bold[red]}$0: fetch updates failed !!$reset_color"; return 1; }
	echo
	pacaur -Qu || { echo "${fg_bold[green]}$0: no updates.$reset_color" ; return 1; }
}

# Fetch and Install updates + aur
pacupg() {
	pacupmir || { echo "$0: pacupmir failed"; return 1; }
	echo "$0: upgrading..."
	## `pacaur -Syu` does not catch pacman errors and continues with AUR packages silently
	## and `pacaur -Syur` exits 1 ?
	sudo pacman -Syu --noconfirm || { echo "${fg_bold[red]}$0: Update FAILED !!$reset_color"; return 1; }
	echo "$0: upgrading..."
	pacaur -Syu --noconfirm --noedit || { echo "${fg_bold[red]}$0: upgrade failed !!$reset_color"; return 1; }
}

# pkgfile wrap
# /usr/share/doc/pkgfile/command-not-found.zsh
pacfind() {
	local pkgs
	local cmd="$1"

	which pkgfile > /dev/null
	if [[ $? -ne 0 ]]
	then
		echo "pkgfile not installed"
		return 125
	fi

	local cachefile="/var/cache/pkgfile/core.files"
	local age=$(( $(date +%s) - $(date +%s -r "$cachefile") ))
	local maxage=$((3600 * 24 * 30)) ## 30 days
	local maxage=$((3600 * 1)) ## 1 hour
	if [[ $age -gt $maxage ]]
	then
		echo "pkgfile cache is ${fg_bold[red]}$(durationtostr $age old)${reset_color} ! run ${fg_bold[red]}sudo pkgfile -u${reset_color}"
	fi

	pkgs=(${(f)"$(pkgfile -b -v -- "$cmd" 2>/dev/null)"})
	if [[ -n "$pkgs" ]]; then
		printf '%s may be found in the following packages:\n' "$cmd"
		printf '  %s\n' $pkgs[@]
		return 0
	fi

	return 127
}
