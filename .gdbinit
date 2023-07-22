
set print pretty on
set print object on
set print static-members off
set print array on
set print array-indexes on
set print vtbl on
set print demangle on
set print asm-demangle on
set print frame-arguments none
set list 25
set demangle-style gnu-v3

# `step` stops on functions with no debug info
set step-mode on
# log `skip`d locations
set debug skip on

set debuginfod enable on

set print inferior-events on

set confirm off
set pagination off

# stop all threads
set non-stop off

set disassembly-flavor intel

set history save on
set history size 2048
set history remove-duplicates 1

set python print-stack full

handle SIGPWR nostop
handle SIGXCPU nostop

python
import sys
import os.path
## @FIXME: hardcoded path
ueprinters = '/home/jo/ps/UnrealEngine/UnrealEngine/Engine/Extras/GDBPrinters/'
if os.path.isdir(ueprinters):
    print("+UE printers: " + ueprinters)
    sys.path.append(ueprinters)
    from UE4Printers import register_ue4_printers
    register_ue4_printers(None)
else:
    print("-UE not found: " + ueprinters)
end

## still requires debug libs:
## $> export LD_LIBRARY_PATH=$VULKAN_SDK/../source/lib:$LD_LIBRARY_PATH
python
import os
import os.path
vksdk = os.environ.get('VULKAN_SDK')
if vksdk is not None and os.path.isdir(vksdk + '/../source/layers'):
    print("+Vulkan directories: $VULKAN_SDK=" + vksdk)
    gdb.execute('directory ' + vksdk + '/../source/layers')
    gdb.execute('directory ' + vksdk + '/../source/loader')
    gdb.execute('directory ' + vksdk + '/../examples')
else:
    print("-Vulkan not found: $VULKAN_SDK=" + str(vksdk))
end

source ~/.gdbinit.local
