#!/bin/zsh

alias pacman='CC=cc CXX=c++ CPP=c++ pacman'
alias pacaur='CC=cc CXX=c++ CPP=c++ pacaur'
alias pac='CC=cc CXX=c++ CPP=c++ pacaur'

if ! command which pacaur > /dev/null
then
	echo "${fg_bold[red]}pacaur not found${reset_color}"
fi

# Update mirror list if necessary
pacupmir() {
	local pacmirrorfile=/etc/pacman.d/mirrorlist
	local pacmirrorfile_expire_sec=$((4 * 24 * 60 * 60)) # 4 days
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

	## `pacaur` does not catch pacman errors and continues with AUR packages silently, so run pacman alone
	echo "\n${fg_bold[green]}$0: pacman -Syuw ...$reset_color\n"
	sudo pacman -Syuw --noconfirm || { echo "${fg_bold[red]}$0: pacman -Syuw failed !!$reset_color"; return 1; }
	echo "\n${fg_bold[green]}$0: pacaur --aur -Syuw ...$reset_color\n"
	pacaur --aur -Syuw --noconfirm --noedit || { echo "${fg_bold[red]}$0: pacaur --aur -Syuw failed !!$reset_color"; return 1; }

	echo
	pacaur -Qu || { echo "${fg_bold[green]}$0: no updates.$reset_color" ; return 1; }
	echo "${fg_bold[green]}$0: pacaur -Syuw OK$reset_color"
}

# Fetch and Install updates + aur
pacupg() {
	pacupmir || { echo "$0: pacupmir failed"; return 1; }

	## `pacaur` does not catch pacman errors and continues with AUR packages silently, so run pacman alone
	## and `pacaur -Syur` exits 1 ?
	echo "\n${fg_bold[green]}$0: pacman -Syu...$reset_color\n"
	sudo pacman -Syu --noconfirm || { echo "${fg_bold[red]}$0: pacman -Syu failed !!$reset_color"; return 1; }
	echo "\n${fg_bold[green]}$0: pacaur --aur -Syu...$reset_color\n"
	pacaur --aur -Syu --noconfirm --noedit || { echo "${fg_bold[red]}$0: pacaur --aur -Syu failed !!$reset_color"; return 1; }
	echo "${fg_bold[green]}$0: pacaur -Syu OK$reset_color"

	checkpacnew
}

checkpacnew() {
	local pacnews=""
	local pacnewcount=0
	grep pacnew /var/log/pacman.log | \
		sed -n -E 's#^.* ([^ ]+\.pacnew)$#\1#gp' | \
		sort -u | \
		while read pacnew
		do
			if [[ -f "$pacnew" ]]
			then
				pacnews="$pacnews $pacnew"
				pacnewcount=$(($pacnewcount + 1))
			fi
		done
	if [[ pacnewcount -gt 0 ]]
	then
		echo
		echo "${fg_bold[red]}/!\\ Found $pacnewcount pacnew$([[ $pacnewcount = 1 ]] || echo s):$pacnews$reset_color"
		echo
	fi
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
