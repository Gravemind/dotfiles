#!/bin/zsh

durationtostr() {
  local t=$1
  local years=$(($t/31556736))
  t=$(($t - $years * 31556736))
  local months=$(($t/2630016))
  t=$(($t - $months * 2630016))
  local days=$(($t/86400))
  t=$(($t - $days * 86400))
  local hours=$(($t/3600))
  t=$(($t - $hours * 3600))
  local mins=$(($t/60))
  t=$(($t - $mins * 60))
  local secs=$t
  [[ "$1" -ge 31556736 && true ]] && echo -n "${years}y "
  [[ "$1" -ge 2630016 && "$1" -lt $((31556736 * 5)) ]] && echo -n "${months}m "
  [[ "$1" -ge 86400 && "$1" -lt $((2630016 * 5))  ]] && echo -n "${days}d "
  [[ "$1" -ge 3600 && "$1" -lt $((86400 * 5)) ]] && echo -n "${hours}h "
  [[ "$1" -ge 60 && "$1" -lt $((3600 * 5)) ]] && echo -n "${years}m "
  [[ "$1" -ge 0 && "$1" -lt $((60 * 5)) ]] && echo -n "${years}s "
  echo "$2"
}
