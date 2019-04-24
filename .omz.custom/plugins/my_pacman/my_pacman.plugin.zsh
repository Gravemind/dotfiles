#!/bin/zsh

export PACPEND='-g explicit -s name,vdiff -H linux,emacs,nvidia'

if ! command which yay > /dev/null
then
	echo "${fg_bold[red]}yay not found${reset_color}"
fi

pacman() env -u CC -u CXX -u CPP $(which -p pacman) "$@"
pacaur() env -u CC -u CXX -u CPP $(which -p pacaur) "$@"
yay()    env -u CC -u CXX -u CPP $(which -p yay) "$@"

alias pac='yay'

# Makes run-help (Alt-H) on yay run `man yay` then `man pacman`
run-help-yay() {
	man yay
	builtin print -nP "%SPress any key for more help or q to quit%s"
	builtin read -k what
	[[ $what != $newline ]] && echo
	[[ $what == [qQ] ]] && return
	man pacman
}

# Update mirror list if out dated
pacupmir() {
	local pacmirrorfile_expire_days=9

	local pacmirrorfile=/etc/pacman.d/mirrorlist
	local pacmirrorfile_expire_sec=$(($pacmirrorfile_expire_days * 24 * 60 * 60))
	local ago=$(( $(date +%s) - $(date +%s -r "$pacmirrorfile") ))
	#local ago=10000
	if [[ "$1" = "-f" || $ago -gt $pacmirrorfile_expire_sec ]]
	then
		echo "${fg_bold[yellow]}mirrorlist is $(durationtostr $ago old), updating now:$reset_color"
		pacupdatemirrors || { echo "${fg_bold[red]}$0: pacupdatemirrors failed !!$reset_color"; return 1 }
	else
		echo "${fg_bold[black]}mirrorlist is $(durationtostr $ago old), updating at $(durationtostr $pacmirrorfile_expire_sec old).$reset_color"
	fi
}

pacu() {
	echo "${fg_bold[blue]}check only package updates...$reset_color"

	# same as default of checkupdates
	export CHECKUPDATES_DB="${TMPDIR:-/tmp}/checkup-db-${USER}/"

	checkupdates > /dev/null
	~/bin/pacpend --aur

	if [[ $? -eq 0 ]]
	then
		#echo "${fg_bold[green]}$0: new pending packages$reset_color"
	else
		echo "${fg_bold[green]}no new packages$reset_color"
	fi
}

pacup() {
	echo "${fg_bold[blue]}check and download only package updates...$reset_color"

	pacupmir || { echo "$0: pacupmir failed"; return 1; }

	cachedir="/var/cache/pacman/pkg/"
	if [[ ! -w "$cachedir" ]]
	then
		echo "$0: Cannot write cache dir as user, it should be safe to:"
		echo "$0: $> sudo chmod a+w $cachedir"
		return 1
	fi

	# same as checkupdate's default
	export CHECKUPDATES_DB="${TMPDIR:-/tmp}/checkup-db-${USER}/"

	if [[ ! -e "$CHECKUPDATES_DB" ]]
	then
		# (uses CHECKUPDATES_DB)
		checkupdates > /dev/null
	fi

	## (`pacaur` does not catch pacman errors and continues with AUR packages silently, so run pacman alone)
	( fakeroot -- pacman -Syuw --noconfirm --dbpath "$CHECKUPDATES_DB" --color=always || { echo "${fg_bold[red]}$0: pacman -Syuw failed !!$reset_color"; return 1; } )
	# ( pacaur --aur -Suw --noconfirm --noedit--color=always || { echo "${fg_bold[red]}$0: pacaur --aur -Syuw failed !!$reset_color"; return 1; } ) | grep -v '^$'

	# (uses CHECKUPDATES_DB)
	~/bin/pacpend --aur

	if [[ $? -eq 0 ]]
	then
		#echo "${fg_bold[green]}$0: new pending packages$reset_color"
	else
		echo "${fg_bold[green]}no new packages$reset_color"
	fi
}

pacupg() {
	echo "${fg_bold[blue]}check, download, and install package updates...$reset_color"

	pacupmir || { echo "$0: pacupmir failed"; return 1; }

	yay -Sy > /dev/null

	~/bin/pacpend -b '' --aur

	## `pacaur` does not catch pacman errors and continues with AUR packages silently, so run pacman alone
	## and `pacaur -Syur` exits 1 ?
	#sudo pacman -Syu --noconfirm "$@" || { echo "${fg_bold[red]}$0: pacman -Syu failed !!$reset_color"; return 1; }
	#pacaur --aur -Syu --noconfirm --noedit "$@" || { echo "${fg_bold[red]}$0: pacaur --aur -Syu failed !!$reset_color"; return 1; }

	date="$(date -Im)"

	yay -Syu --noconfirm "$@" || { echo "${fg_bold[red]}$0: yay -Syu failed !!$reset_color"; return 1; }

	checkpacnew
	echo "${fg_bold[green]}packages updated$reset_color";

	# {
	# 	echo "${fg_bold[green]}packages updated$reset_color";
	# 	# checkpacnew; # breaks less !?
	# 	\cat <(checkpacnew);
	# 	paclog --after="$date" --color;
	# } | less --quit-if-one-screen --RAW-CONTROL-CHARS --no-init
}

checkpacnew() {
	local pacnews=()
	while read -u3 -r -d $'\n' pacnew
	do
		if [[ -e "$pacnew" ]]
		then
			pacnews+=("$pacnew")
		fi
	done 3< <( grep --binary-files=text pacnew /var/log/pacman.log |
				   sed -n -E 's#^.*installed as (.*\.pacnew)$#\1#gp' |
				   sort -u )
	if [[ "${#pacnews[@]}" -gt 0 ]]
	then
		echo
		echo "${fg_bold[red]}$0: found ${#pacnews[@]} pacnew: ${pacnews[@]}.$reset_color"
		echo
	else
		echo "${fg_bold[black]}$0: found 0 pacnew.$reset_color"
	fi
}

pacfind() {
	local cachefile="/var/lib/pacman/sync/core.files"
	local maxage=$((3600 * 24 * 30)) ## 30 days

	local age=-1
	if [[ -f "$cachefile" ]]; then
		age=$(( $(date +%s) - $(date +%s -r "$cachefile") ))
	fi
	if [[ $age -lt 0 || $age -gt $maxage ]]
	then
		echo "$0: pacman files db is ${fg_bold[red]}$(durationtostr $age old)${reset_color} ! updating: pac -Fy:"
		pac -Fy
	else
		echo "$0: pacman files db is $(durationtostr $age old) (updating at $(durationtostr $maxage old))"
	fi

	pac -Fs "$@"
}

# pkgfile wrap
# /usr/share/doc/pkgfile/command-not-found.zsh
pkgpacfind() {
	local pkgs
	local cmd="$1"

	which pkgfile > /dev/null
	if [[ $? -ne 0 ]]
	then
		echo "$0: pkgfile not installed"
		return 125
	fi

	local cachefile="/var/cache/pkgfile/core.files"
	if [[ ! -e "$cachefile" ]]
	then
		echo "$0: no pkgfile cache ! running sudo pkgfile -u:"
		sudo pkgfile -u
	else
		local age=$(( $(date +%s) - $(date +%s -r "$cachefile") ))
		local maxage=$(( 3600 * 24 * 30 )) ## 30 days
		if [[ $age -gt $maxage ]]
		then
			echo "$0: pkgfile cache is ${fg_bold[red]}$(durationtostr $age old)${reset_color} ! running sudo pkgfile -u:"
			sudo pkgfile -u
		else
			echo "$0: pkgfile cache is $(durationtostr $age old) (updating at $(durationtostr $maxage old))"
		fi
	fi

	#args="-b" # bin sbin only

	pkgs=(${(f)"$(pkgfile -v $args -- "$cmd" 2>/dev/null)"})
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

# lists last installed packages
paclast() {
	local lastn=40
	echo "Last $lastn packages installed explicitly:"
	echo "ago:"$'\t'"name:"
	local pkgdate=
	local pkgname=
	while read -r pkgdate pkgname
	do
		local ago=$(( $(date +%s) - pkgdate ))
		echo "$(durationtostr $ago)"$'\t'"$pkgname"
	done < <(expac --timefmt=%s '%w\t%l\t%n' | grep '^explicit' | cut -f 2- | sort -n | tail -$lastn)
}
