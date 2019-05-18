
#nv_metamode_dp_4="+1680+0"
#nv_metamode_hdmi_0="+0+0"

nv_metamode_dp_4="+0+0"
nv_metamode_hdmi_0="+1920+0"

nvdual() {
	nvidia-settings --assign 'CurrentMetaMode=DP-4: nvidia-auto-select '"$nv_metamode_dp_4"' {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}, HDMI-0: nvidia-auto-select '"$nv_metamode_hdmi_0"' {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}'
}

nvdualunsync() {
	nvidia-settings --assign 'CurrentMetaMode=DP-4: nvidia-auto-select '"$nv_metamode_dp_4"' {ForceCompositionPipeline=Off, ForceFullCompositionPipeline=Off}, HDMI-0: nvidia-auto-select '"$nv_metamode_hdmi_0"' {ForceCompositionPipeline=Off, ForceFullCompositionPipeline=Off}'
}

nvsingle() {
	nvidia-settings --assign 'CurrentMetaMode=DP-4: nvidia-auto-select +0+0 {ForceCompositionPipeline=Off, ForceFullCompositionPipeline=Off}'
}

nvsinglesync() {
	nvidia-settings --assign 'CurrentMetaMode=DP-4: nvidia-auto-select +0+0 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}'
}

nvoverclock() {
	# Power limit
	# query Max Power Limit with: nvidia-smi -q | grep Power
	sudo nvidia-smi -pl 216

	# clock offsets
	# tested `gputest /test=fur`: Clock+200:crash, Mem+700:artefacts
	nvidia-settings -a "[gpu:0]/GPUMemoryTransferRateOffset[3]=500"
	nvidia-settings -a "[gpu:0]/GPUGraphicsClockOffset[3]=150"

	# GPUPowerMizerMode
	# - 0 adaptive
	# - 1 high perf
	# - 2 auto
	nvidia-settings -a "GPUPowerMizerMode=1"
}


nvoverclockoff() {
	sudo nvidia-smi -pl 200
	nvidia-settings -a "[gpu:0]/GPUMemoryTransferRateOffset[3]=0"
	nvidia-settings -a "[gpu:0]/GPUGraphicsClockOffset[3]=0"
	nvidia-settings -a "GPUPowerMizerMode=2"
}
