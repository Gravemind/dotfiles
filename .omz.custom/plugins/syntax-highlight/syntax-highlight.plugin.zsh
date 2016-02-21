
ZSH_HIGHLIGHT_HIGHLIGHTERS=(line main)

source "${0:a:h}/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# add black background to all styles
for k in "${(@k)ZSH_HIGHLIGHT_STYLES}"; do
	ZSH_HIGHLIGHT_STYLES[$k]="bg=black,$ZSH_HIGHLIGHT_STYLES[$k]"
done
