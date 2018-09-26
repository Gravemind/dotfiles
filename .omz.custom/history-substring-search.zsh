
# skip duplicate _adjacent_ search results
#setopt HIST_FIND_NO_DUPS
# skip all duplicates
setopt HIST_IGNORE_ALL_DUPS

# case sensitive
HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS=I
# case insensitive
#HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS=i
# lower match lower and upper, upper match upper only
#HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS=l
# fuzzy: `ab c` will match `*ab*c*`
HISTORY_SUBSTRING_SEARCH_FUZZY=1
