#!/bin/zsh

# eg: "1yr 10mo" for 1 year and 10 months
durationtostr_old() {
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
  [[ "$1" -ge 31556736 && true ]] && echo -n "${years}yr "
  [[ "$1" -ge 2630016 && "$1" -lt $((31556736 * 5)) ]] && echo -n "${months}mo "
  [[ "$1" -ge 86400 && "$1" -lt $((2630016 * 5))  ]] && echo -n "${days}da "
  [[ "$1" -ge 3600 && "$1" -lt $((86400 * 5)) ]] && echo -n "${hours}h "
  [[ "$1" -ge 60 && "$1" -lt $((3600 * 5)) ]] && echo -n "${years}m "
  [[ "$1" -ge 0 && "$1" -lt $((60 * 5)) ]] && echo -n "${years}s "
  echo "$2"
}

# decimal-style: "1.9yr" for 1 year and 0.9 of a year (1 year and 10 months)
durationtostr() {
  if [[ "$1" -ge 31556736 ]]
  then
	  div=31556736
	  unit='yr'
  elif [[ "$1" -ge 2629746 ]]
  then
	  div=2629746
	  unit='mo'
  elif [[ "$1" -ge 86400 ]]
  then
	  div=86400
	  unit='da'
  elif [[ "$1" -ge 3600 ]]
  then
	  div=3600
	  unit='h'
  elif [[ "$1" -ge 60 ]]
  then
	  div=60
	  unit='m'
  else
	  div=1
	  unit='s'
  fi
  v="$(echo "scale=1; $1 / $div" | bc)"
  echo "$v$unit" "$2"
}
