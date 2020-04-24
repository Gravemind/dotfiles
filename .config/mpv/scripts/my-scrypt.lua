-- my-scrupt.lua

-- Manages profiles 'scaletempo' and 'svp'

local mp = require 'mp'

local profiles_enabled = {
    scaletempo = false,
    svp = true,
}

local function enable_profile(name, enable)
    profiles_enabled[name] = enable
    local profile
    if enable then
        profile = "enable-"..name
    else
        profile = "disable-"..name
    end
    --mp.osd_message(profile)
    print(profile)
    mp.commandv("apply-profile", profile)
end

local function on_speed_changed(name, value)
    if value == 1.0 then
        enable_profile("scaletempo", false)
        if svp_was_enabled then
            enable_profile("svp", true)
        end
    elseif value > 0.99 and value < 1.01 then
        mp.set_property_native("speed", 1.0)
    else
        enable_profile("scaletempo", true)
        svp_was_enabled = profiles_enabled.svp
        enable_profile("svp", false)
    end
end

local function toggle_svp()
    enable_profile("svp", not profiles_enabled.svp)
end

for k, v in pairs(profiles_enabled) do
    if os.getenv(string.upper(k)) == "0" then
        enable_profile(k, false)
    else
        enable_profile(k, profiles_enabled[k])
    end
end
local svp_was_enabled = profiles_enabled.svp

mp.observe_property("speed", "number", on_speed_changed)

mp.add_key_binding("n", "toggle-svp", toggle_svp, {repeatable=true})
