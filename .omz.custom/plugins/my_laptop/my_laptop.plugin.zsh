#!/bin/zsh

alias bat='acpi -bV'
alias wscan='iwlist wlan0 scanning'

nvhdmionly() {
	nvidia-settings --assign 'CurrentMetaMode=HDMI-0: nvidia-auto-select +0+0 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}'
	sleep 0.2
	## must set dpi with LVDS-1-1 enabled or screen freeze (nv bug?)
	xrandr --output LVDS-1-1 --auto --output HDMI-0 --auto --dpi 94
	sleep 0.2
	xset dpms force off
	sleep 0.2
	xrandr --output LVDS-1-1 --off
	sleep 0.2
}

nvhdmionlyunsync() {
	nvidia-settings --assign 'CurrentMetaMode=HDMI-0: nvidia-auto-select +0+0 {ForceCompositionPipeline=Off, ForceFullCompositionPipeline=Off}'
	sleep 0.2
	## must set dpi with LVDS-1-1 enabled or screen freeze (nv bug?)
	xrandr --output LVDS-1-1 --auto --output HDMI-0 --auto --dpi 94
	sleep 0.2
	xset dpms force off
	sleep 0.2
	xrandr --output LVDS-1-1 --off
	sleep 0.2
}
