
nvdual() {
    nvidia-settings --assign "CurrentMetaMode=DPY-1: nvidia-auto-select @1680x1050 +0+0 {ViewPortIn=1680x1050, ViewPortOut=1680x1050+0+0, ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}, DPY-6: nvidia-auto-select @1920x1200 +1680+0 {ViewPortIn=1920x1200, ViewPortOut=1920x1200+0+0, ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}"
}

nvdualunsync() {
    nvidia-settings --assign "CurrentMetaMode=DPY-1: nvidia-auto-select @1680x1050 +0+0 {ViewPortIn=1680x1050, ViewPortOut=1680x1050+0+0, ForceCompositionPipeline=Off, ForceFullCompositionPipeline=Off}, DPY-6: nvidia-auto-select @1920x1200 +1680+0 {ViewPortIn=1920x1200, ViewPortOut=1920x1200+0+0, ForceCompositionPipeline=Off, ForceFullCompositionPipeline=Off}"
}

nvsingle() {
    nvidia-settings --assign "CurrentMetaMode=DPY-6: nvidia-auto-select @1920x1200 +0+0 {ViewPortIn=1920x1200, ViewPortOut=1920x1200+0+0}"
}

nvsinglesync() {
    nvidia-settings --assign "CurrentMetaMode=DPY-6: nvidia-auto-select @1920x1200 +0+0 {ViewPortIn=1920x1200, ViewPortOut=1920x1200+0+0, ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}"
}
