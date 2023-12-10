
-- Download subtitles with subdl

-- https://github.com/fullmetalsheep/mpv-iina-scripts/blob/master/trueautosub.lua
-- https://github.com/wiiaboo/mpv-scripts/blob/master/subit.lua
-- https://codeberg.org/NRK/mpv-toolbox/src/branch/master/mdmenu/mdmenu.lua

local utils = require 'mp.utils'

local function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

function get_file_name(file)
      return file:match("[^/]+$")
end

function tmpdir()
    local d = os.getenv("TMPDIR")
    if not d then
        d = "/tmp"
    end
    return d
end

local function list()
    local subdl = 'subdl'
    local path = mp.get_property("path")
    local t = {}
    t.args = {'bash', '-c', '"$1" --download=none "$2" | grep "^#" | rofi -dmenu -p "subdl $2"', '--', subdl, path}
    t.capture_stdout = true
    local res = utils.subprocess(t)
    if res.status ~= 0 then
        mp.osd_message("subd query failed")
        return
    end
    local id = res.stdout:match('^#(%d+) ')
    local ext = res.stdout:match('(%.%w+)[ \n]*$')
    local output = tmpdir() .. '/subdl.' .. get_file_name(path) .. ext
    print('Downloading subdl ' .. path .. ' #' .. id .. ' to ' .. output)
    local t = {}
    t.args = {'bash', '-c', '"$1" --download="$3" --existing=overwrite --output="$4" "$2"', '--', subdl, path, id, output}
    local res = utils.subprocess(t)
    if res.status ~= 0 then
        mp.osd_message("subd download failed")
        return
    end
    mp.osd_message("subdl downloaded to " .. output)
    mp.commandv("sub-add", output, "select")
end

local function subdl()
    list()
end

mp.add_key_binding("Ctrl+j", "subdl", subdl)
