-- speed.lua

-- Applies profiles when play speed changes: applies "speedx1" profile when
-- playing at x1, else applies "speedxn".

local mp = require 'mp'

local speedxn_profile = "speedxn"
local speedx1_profile = "speedx1"

local last_set_profile = nil
local function set_profile(prof)
    if prof ~= last_set_profile then
        last_set_profile = prof
        print("set profile "..prof)
        mp.commandv("apply-profile", prof)
    end
end

local function changed_speed(name, value)
    if value == 1.0 then
        set_profile(speedx1_profile)
    elseif value > 0.99 and value < 1.01 then
        mp.set_property_native("speed", 1.0)
    else
        set_profile(speedxn_profile)
    end
end

mp.observe_property("speed", "number", changed_speed)
