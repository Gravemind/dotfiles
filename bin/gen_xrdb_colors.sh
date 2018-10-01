#!/bin/bash

set -euo pipefail

# Usage example:
#  - generate colors in gpick (order: red gren blue yellow cyan magenta)
#  - select and Ctrl-c them from gpick
#  - run: xrdb ~/.Xresources ; xrdb -merge <( gen_xrdb_colors <(xclip -o) ) ; urxvt

col_nums=(
    1 2 4       # red gren blue
    3 6 5       # yellow cyan magenta
    9 10 12     # (bold) red gren blue
    11 14 13    # (bold) yellow cyan magenta
)

input_file="${1}"

input=()
while read -r col
do
    # echo "col $col" 1>&2
    col="${col%%;*}"
    [[ -n "$col" ]] || continue
    [[ "$col" =~ ^#.* ]] || col="#$col"
    input+=("$col")
done < <( cat "$input_file" ; echo )

if [[ "${#input[@]}" -eq 6 ]]
then
    input+=( "${input[@]}" )
fi

i=0
for col in "${input[@]}"
do
    l="*color${col_nums[i]}: $col"
    echo "$l" 1>&2
    echo "$l"
    i=$((i + 1))
done
