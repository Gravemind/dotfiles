
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
  (( $D > 0 )) && printf '%dd ' $D
  (( $H > 0 )) && printf '%dh ' $H
  (( $M > 0 )) && printf '%dm ' $M
  (( $S > 0 )) && printf '%ds ' $S
  printf '\n'
}

# Update mirror list if necessary
pacupmir() {
	local ago=$(( $(date +%s) - $(date +%s -r "$pacmirrorfile") ))
	echo "Mirrors last updated $(duration2s $ago)ago (update every $(duration2s $pacmirrorfile_expire_sec))"
	if [[ $since -gt $pacmirrorfile_expire_sec ]]
	then
		sudo pacupdatemirrors
	fi
}

# Fetch only new updates
pacup() {
	pacupmir
	yaourt -Syuwq --noconfirm || echo "Update FAILED !!"
	echo
	yaourt -Qu || echo "No updates."
}

# Fetch and Install updates + aur
pacupg() {
	pacupmir
	yaourt -Syua --noconfirm || echo "Update FAILED !!"
}
