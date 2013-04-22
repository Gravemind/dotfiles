# correct commands, but not any arguments (correct_all would do that)
if [[ "$DISABLE_CORRECTION" != "true" ]]
then
	setopt correct
fi

