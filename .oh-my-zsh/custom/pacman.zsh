
alias pacman='pacman --color=auto '
compdef _pacman yaourt=pacman

pacmirrorfile=/etc/pacman.d/mirrorlist
pacmirrorfile_expire_sec=$((4 * 24 * 60 * 60)) # 4 days

_subdurstr() {
  STR=""
  (( $1 > 0 )) && STR="${STR}$1 $2"
  (( $1 > 1 )) && STR="${STR}s"
  (( $1 > 0 )) && STR="${STR} "
  echo "$STR"
}

durationtostr() {
  local T=$1
  local D=$((T/60/60/24))
  local H=$((T/60/60%24))
  local M=$((T/60%60))
  local S=$((T%60))
  STR="$(_subdurstr $D day)$(_subdurstr $H hour)"
  if [[ -n "$STR" ]]
  then
	  echo "$STR$2"
  else
	  STR="$(_subdurstr $M min)$(_subdurstr $S second)"
	  echo "$STR$2"
  fi
}

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
	pacupmir
	echo "$0: fetching updates..."
	yaourt -Syuw --noconfirm || echo "${fg_bold[red]}$0: update FAILED !!$reset_color"
	echo
	yaourt -Qu || echo "${fg_bold[green]}$0: no updates.$reset_color"
}

# Fetch and Install updates + aur
pacupg() {
	pacupmir
	echo "$0: upgrading..."
	yaourt -Syua --noconfirm || echo "${fg_bold[red]}$0: Update FAILED !!$reset_color"
}
