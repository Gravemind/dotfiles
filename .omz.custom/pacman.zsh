#!/bin/zsh

alias pacman='pacman --color=auto '
alias yaourt='yaourt '
compdef _pacman yaourt=pacman

pacmirrorfile=/etc/pacman.d/mirrorlist
pacmirrorfile_expire_sec=$((4 * 24 * 60 * 60)) # 4 days

# Update mirror list if necessary
pacupmir() {
	local ago=$(( $(date +%s) - $(date +%s -r "$pacmirrorfile") ))
	#local ago=10000
	if [[ $ago -gt $pacmirrorfile_expire_sec ]]
	then
		echo "$0: updating... (${fg_bold[green]}$(durationtostr $ago old)${reset_color})"
		sudo pacupdatemirrors || echo "${fg_bold[red]}$0: update FAILED !!$reset_color"
	else
		echo "$0: up to date (${fg_bold[cyan]}$(durationtostr $ago old)${reset_color})"
	fi
}

# Fetch only new updates
pacup() {
	CC=
	CXX=
	sudo echo "$0 ..."
	pacupmir
	echo "$0: fetching updates..."
	yaourt -Syuw --noconfirm || echo "${fg_bold[red]}$0: update FAILED !!$reset_color"
	echo
	yaourt -Qu || echo "${fg_bold[green]}$0: no updates.$reset_color"
}

# Fetch and Install updates + aur
pacupg() {
	CC=
	CXX=
	sudo echo "$0 ..."
	pacupmir
	echo "$0: upgrading..."
	yaourt -Syua --noconfirm || echo "${fg_bold[red]}$0: Update FAILED !!$reset_color"
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
