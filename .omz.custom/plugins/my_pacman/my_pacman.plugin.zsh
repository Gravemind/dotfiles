#!/bin/zsh

alias pacman='env -u CC -u CXX -u CPP pacman '
alias pacaur='env -u CC -u CXX -u CPP pacaur '
alias pac='env -u CC -u CXX -u CPP pacaur '

if ! command which pacaur > /dev/null
then
	echo "${fg_bold[red]}pacaur not found${reset_color}"
fi

# Update mirror list if out dated
pacupmir() {
	local pacmirrorfile_expire_days=9

	local pacmirrorfile=/etc/pacman.d/mirrorlist
	local pacmirrorfile_expire_sec=$(($pacmirrorfile_expire_days * 24 * 60 * 60))
	local ago=$(( $(date +%s) - $(date +%s -r "$pacmirrorfile") ))
	#local ago=10000
	if [[ "$1" = "-f" || $ago -gt $pacmirrorfile_expire_sec ]]
	then
		echo "$0: mirrorlist is $(durationtostr $ago old), updating:"
		pacupdatemirrors || { echo "${fg_bold[red]}$0: pacupdatemirrors failed !!$reset_color"; return 1 }
	else
		#echo "$0: mirrorlist is $(durationtostr $ago old), not updating yet"
	fi
}

# Download only new packages
pacup() {
	pacupmir || { echo "$0: pacupmir failed"; return 1; }

	cachedir="/var/cache/pacman/pkg/"
	if [[ ! -w "$cachedir" ]]
	then
		echo "$0: Cannot write cache dir as user, it should be safe to:"
		echo "$0: $> sudo chmod a+w $cachedir"
		return 1
	fi

	# same as default of checkupdates
	export CHECKUPDATES_DB="${TMPDIR:-/tmp}/checkup-db-${USER}/"

	checkupdates > /dev/null

	~/bin/pacpend

	## `pacaur` does not catch pacman errors and continues with AUR packages silently, so run pacman alone
	( fakeroot -- pacman -Suw --noconfirm --dbpath "$CHECKUPDATES_DB" --color=always || { echo "${fg_bold[red]}$0: pacman -Syuw failed !!$reset_color"; return 1; } ) | grep -v '^$'
	( pacaur --aur -Suw --noconfirm --noedit--color=always || { echo "${fg_bold[red]}$0: pacaur --aur -Syuw failed !!$reset_color"; return 1; } ) | grep -v '^$'

	~/bin/pacpend
	if [[ $? -eq 0 ]]
	then
		#echo "${fg_bold[green]}$0: new pending packages$reset_color"
	else
		echo "${fg_bold[green]}$0: no new packages$reset_color"
	fi
}

# Download and Install packages
pacupg() {
	pacupmir || { echo "$0: pacupmir failed"; return 1; }

	pacman -Sy > /dev/null
	~/bin/pacpend -b ''

	## `pacaur` does not catch pacman errors and continues with AUR packages silently, so run pacman alone
	## and `pacaur -Syur` exits 1 ?
	sudo pacman -Syu --noconfirm "$@" || { echo "${fg_bold[red]}$0: pacman -Syu failed !!$reset_color"; return 1; }
	pacaur --aur -Syu --noconfirm --noedit "$@" || { echo "${fg_bold[red]}$0: pacaur --aur -Syu failed !!$reset_color"; return 1; }

	echo "${fg_bold[green]}$0: package updated$reset_color"

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
		echo "${fg_bold[red]}$0: found $pacnewcount pacnew:$reset_color$pacnews"
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
		echo "$0: pkgfile not installed"
		return 125
	fi

	local cachefile="/var/cache/pkgfile/core.files"
	local age=$(( $(date +%s) - $(date +%s -r "$cachefile") ))
	local maxage=$((3600 * 24 * 30)) ## 30 days
	if [[ $age -gt $maxage ]]
	then
		echo "$0: pkgfile cache is ${fg_bold[red]}$(durationtostr $age old)${reset_color} ! running sudo pkgfile -u:"
		sudo pkgfile -u
	else
		echo "$0: pkgfile cache is $(durationtostr $age old) (updating at $(durationtostr $maxage old))"
	fi

	pkgs=(${(f)"$(pkgfile -b -v -- "$cmd" 2>/dev/null)"})
	if [[ -n "$pkgs" ]]; then
		printf '%s may be found in the following packages:\n' "$cmd"
		printf '  %s\n' $pkgs[@]
		return 0
	else
		printf '%s not found.\n' "$cmd"
		return 1
	fi

	return 127
}
