
alias redx='redshift -x'
#alias red1='redshift -O 5500 -b 0.8'
#alias red2='redshift -O 4000 -b 0.5'


function red() {
	if [[ "$1" = '-h' ]]
	then
		echo "$0 [-h] [screenID]"
		echo "    a|s  : decrease redshift"
		echo "    d|w  : increase redshift"
		echo "    x    : reset redshift and quit"
		echo '    q|\\n : quit'
		return
	fi

	opt="-mrandr"
	if [[ -n "$1" ]]
	then
		opt="-mrandr:crtc=$1"
	fi
	cola=6500
	bria=100
	colstep=600
	bristep=10
	i=0
	while read -s -k c
	do
		#echo "--$c--"
		case "$c" in
			a|s)
				i=$(($i - 1))
				;;
			d|w)
				i=$(($i + 1))
				;;
			x)
				echo reset
				redshift -x $opt
				return
				;;
			q|''|$'\n')
				echo ok
				return
				;;
			*)
				;;
		esac
		if [[ $i -lt 0 ]]
		then
			col=$(($i * $colstep + $cola))
			bri=$(($i * $bristep + $bria))
			redshift -O $col -b $(($bri / 100.0)) $opt
			echo $col $bri
		else
			i=0
			echo reset
			redshift -x $opt
		fi
	done
}
