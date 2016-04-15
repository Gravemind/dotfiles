#!/bin/zsh

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
