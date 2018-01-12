
set print pretty on
set print object on
set print static-members off
set print array on
set print array-indexes on
set print vtbl on
set print demangle on
set print asm-demangle on
set demangle-style gnu-v3

set print inferior-events on

set confirm off
#set pagination off

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
## @FIXME: harded path
sys.path.append('/home/jo/ps/UnrealEngine/UnrealEngine/Engine/Extras/GDBPrinters/')
from UE4Printers import register_ue4_printers
register_ue4_printers(None)
end

## still requires debug libs:
## $> export LD_LIBRARY_PATH=$VULKAN_SDK/../source/lib:$LD_LIBRARY_PATH
python
import os
vksdk = os.environ.get('VULKAN_SDK')
if vksdk is not None:
    gdb.execute('directory ' + vksdk + '/../source/layers')
    gdb.execute('directory ' + vksdk + '/../source/loader')
    gdb.execute('directory ' + vksdk + '/../examples')
end

source ~/.gdbinit.local
