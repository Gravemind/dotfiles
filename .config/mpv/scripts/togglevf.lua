-- togglevf.lua

-- Binds [N] to "toggle-vf" to enable/disable (save/restore) the current "vf".

local mp = require 'mp'

local last_vf = nil

local function toggle_vf()
    local vf = mp.get_property_native("vf")
    print(vf)
    if last_vf == nil then
        last_vf = vf
        mp.set_property_native("vf", {})
        mp.osd_message("vf removed")
    else
        mp.set_property_native("vf", last_vf)
        last_vf = nil
        mp.osd_message("vf re-enabled")
    end
end

mp.add_key_binding("n", "toggle-vf", toggle_vf, {repeatable=true})
