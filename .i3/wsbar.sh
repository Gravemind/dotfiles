#!/bin/zsh

SLEEP=1
LOW_SLEEP=5

ICONPATH=$HOME/.dzen
INTERFACE=eth0

RXB=`cat /sys/class/net/${INTERFACE}/statistics/rx_bytes`
TXB=`cat /sys/class/net/${INTERFACE}/statistics/tx_bytes`

COL_FG='#7799AA'
COL_FG1='#557799'
COL_FG2='#335566'
COL_FG3='#888888'
COL_OK='#80AA83'
COL_FAIL='#9E3A26'
COL_BG='#000000'
COL_BG1='#222222'

PICO=4
PBAR=5

SEPARATOR="^fg($COL_BG1) ~ "

MEM_AWKS='/^MemTotal:/   {mtotal=$2};
/^MemFree:/    {mfree=$2};
/^Cached:/    {mcached=$2};
END {
    print mtotal-mfree-mcached " " mtotal;
}'

DBAR="/home/jo/documents/repo/dzen-read-only/gadgets/./gdbar"

i=0

## print temp from sensors
## $1 < `sensors` command output
## $2 < temp to match (ex: "CPU Temperature")
## stdout > average of matche temperatures
function print_temp {
	TEMP=$(echo "$1" | sed -rn "s/$2[0-9a-z: ]+[+-]([0-9\.]+).*$/a+=\1\;i+=1; /gp")
	TEMP=$(echo "i=0.0;a=0.0;" "$TEMP" "a/i" | bc)
	TEST=$(echo "$TEMP > 60" | bc)
	O=""
	if [ "$TEST" = "1" ]
	then
		O+="^fg($COL_FAIL)$TEMP"
	else
		O+="^fg($COL_OK)$TEMP"
	fi
	echo $O
}

while true
do
	# this command takes 1 seconde
	CPU=`mpstat -P ALL 1 1 | tail -n 3`
	CPU1=$(echo "$CPU" | grep " all " | awk '{print $11}')

	OUT=""

	#
	# NOTIFILE
	#
	PURPLE=`cat ~/purple_notify`
	if [ $PURPLE -eq 1 ]
	then
		OUT+="test"
	fi

	#
	# CMUS
	#
	CMUSON=`cmus-remote -C status 2> /dev/null`
	if [ "$CMUSON" = "" ]
	then
		OUT+="^fg($COL_BG1)^p(;+$PICO)^i(${ICONPATH}/stop.xbm)^p(;-$PICO)"
	else
		CMUSST=`cmus-remote -C status | grep "status" | cut -d " " -f 2`
		if [ "$CMUSST" != "playing" ]
		then
			OUT+="^fg($COL_FAIL)^p(;+$PICO)^i(${ICONPATH}/pause.xbm)^p(;-$PICO)"
		else
			OUT+="^fg($COL_OK)^p(;+$PICO)^i(${ICONPATH}/play.xbm) ^i(${ICONPATH}/note.xbm)^p(;-$PICO) ^fg($COL_FG1)"
			OUT+=`cmus-remote -C status | grep "tag artist" | cut -d " " -f 3-10`
			OUT+=" ^fg($COL_FG2)"
			OUT+=`cmus-remote -C status | grep "tag title" | cut -d " " -f 3-10`
		fi
	fi

	OUT+="^pa(1113)"

	OUT+="$SEPARATOR"

	#
	# NETWORK
	#
    RXBN=`cat /sys/class/net/${INTERFACE}/statistics/rx_bytes`
    TXBN=`cat /sys/class/net/${INTERFACE}/statistics/tx_bytes`
    RXR=$(printf "%4d" $(echo "($RXBN - $RXB) / 1024" | bc) )
    TXR=$(printf "%4d" $(echo "($TXBN - $TXB) / 1024" | bc) )
    OUT+="^fg($COL_FG1)${RXR}^fg($COL_OK)^p(2;+$PICO)^i(${ICONPATH}/net_down_01.xbm)^p(;-$PICO)"
	OUT+=" ^fg($COL_FG1)${TXR}^fg($COL_FAIL)^p(2;+$PICO)^i(${ICONPATH}/net_up_02.xbm)^p(;-$PICO)"
    RXB=$RXBN
	TXB=$TXBN

	OUT+="$SEPARATOR"

	#
	# CPUs
	#
	OUT+="^fg($COL_FG2)^p(;+$PICO)^i(${ICONPATH}/cpu.xbm)^p(;-$PICO)^p(5;+$PBAR)"
	OUT+=$(echo "100 - $CPU1" | bc | $DBAR -ss 1 -sw 2 -w 50 -h 6 -fg $COL_FG2 -bg $COL_BG1 )
	OUT+="^p(;-$PBAR) "

	#
	# MEM
	#
	if [ $i -eq 0 ]
	then
		OUT_MEM=""
		OUT_MEM+="^fg($COL_FG2)^p(;+$PICO)^i(${ICONPATH}/mem.xbm)^p(;-$PICO)^p(5;+$PBAR)"
		OUT_MEM+=$(awk "$MEM_AWKS" /proc/meminfo | $DBAR -ss 1 -sw 2 -w 50 -h 6 -fg $COL_FG2 -bg $COL_BG1 )
		OUT_MEM+="^p(;-$PBAR)"
	fi
	OUT+="$OUT_MEM"

	OUT+="$SEPARATOR"

	#
	# TEMPERATURES
	#
	if [ $i -eq 0 ]
	then
		SENSORS=`sensors`
		OUT_TEMP=""
		OUT_TEMP+="^fg($COL_FG1)^p(2;+$PICO)^i(${ICONPATH}/temp.xbm)^p(5;-$PICO)"
		OUT_TEMP+=$(print_temp "$SENSORS" "MB Temp")
		OUT_TEMP+="^p(5)"
		OUT_TEMP+=$(print_temp "$SENSORS" "Core")
		OUT_TEMP+="^p(5)"
		OUT_TEMP+=$(print_temp "$SENSORS" "temp1")
		OUT_TEMP+=""
	fi
	OUT+="$OUT_TEMP"

	OUT+="$SEPARATOR"

	#
	# VOLUME
	#
	OUT+="^fg($COL_FG2)^p(;+$PICO)^i(${ICONPATH}/spkr_01.xbm)^p(;-$PICO)^p(5;+$PBAR)"
	VOL=`amixer get Master | grep -Eo "[0-9]+%"`
	OUT+=$(echo $VOL | $DBAR -ss 1 -sw 2 -w 50 -h 6 -fg $COL_FG2 -bg $COL_BG1 )
	OUT+="^p(;-$PBAR)"

	OUT+="$SEPARATOR"

	#
	# DATE
	#
	OUT+="^fg($COL_FG)"
	OUT+=`date -d "now" "+%d/%m %H:%M"`

	#
	# print all
	#
	echo $OUT
	if [ $i -eq $LOW_SLEEP ]
	then
		i=0
	else
		i=$(expr $i + 1)
	fi

done | i3-wsbar --show-all -c "dzen2 -dock -expand left -x %x -fn -*-DejaVu\ Sans\ Mono-normal-normal-normal-*-13-*-*-*-*-*-*-*" --input-on DVI-0 -output-on DVI-0
