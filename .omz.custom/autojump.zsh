
AUTOJUMP_FILES=( '/etc/profile.d/autojump.zsh' '/usr/share/autojump/autojump.zsh' )

for AUTOJUMP_FILE in $AUTOJUMP_FILES
do
	if [ -f "$AUTOJUMP_FILE" ]
	then
		source "$AUTOJUMP_FILE"
		break;
	fi
done

alias js="autojump -s"
alias ji="autojump -i 50"
alias jd="autojump -d 50"
