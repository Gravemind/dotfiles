-- my-scrupt.lua

-- Manages profiles 'scaletempo' and 'svp'

local mp = require 'mp'

local profiles_enabled = {
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
    print(profile)
    mp.commandv("apply-profile", profile)
end

local function toggle_svp()
    enable_profile("svp", not profiles_enabled.svp)
    mp.osd_message("svp " .. tostring(profiles_enabled.svp))
end

local function init()
    -- print('my-script init')
    for k, v in pairs(profiles_enabled) do
        if os.getenv(string.upper(k)) == "0" then
            enable_profile(k, false)
        else
            enable_profile(k, profiles_enabled[k])
        end
    end
    local svp_was_enabled = profiles_enabled.svp

    mp.add_key_binding("n", "toggle-svp", toggle_svp, {repeatable=true})
end

init()
