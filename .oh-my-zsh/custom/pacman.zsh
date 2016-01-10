
alias pacman='pacman --color=auto '
compdef _pacman yaourt=pacman

pacmirrorfile=/etc/pacman.d/mirrorlist
pacmirrorfile_expire_sec=$((4 * 24 * 60 * 60)) # 4 days

duration2s() {
  local T=$1
  local D=$((T/60/60/24))
  local H=$((T/60/60%24))
  local M=$((T/60%60))
  local S=$((T%60))
  (( $D > 0 )) && printf '%d days ' $D
  (( $H > 0 )) && printf '%d hours ' $H
  #(( $M > 0 )) && printf '%dm ' $M
  #(( $S > 0 )) && printf '%ds ' $S
  printf '\n'
}

# Update mirror list if necessary
pacupmir() {
	local ago=$(( $(date +%s) - $(date +%s -r "$pacmirrorfile") ))
	if [[ $ago -gt $pacmirrorfile_expire_sec ]]
	then
		echo "$0: updating... (${fg_bold[green]}$(duration2s $ago)${reset_color}old)"
		sudo pacupdatemirrors || echo "${fg_bold[red]}$0: update FAILED !!$reset_color"
	else
		echo "$0: up to date (${fg_bold[cyan]}$(duration2s $ago)${reset_color}old)"
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
