
echo " ┌─────"
cat ~/TODO.org | sed -r '/^\s*$/d' | sed 's/.*/ │ &/'
echo " └"
