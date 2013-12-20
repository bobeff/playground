--[[
    Can you find any value for f such that the call pcall(pcall, f)
returns false as its first result.
]]

local f = function()
  local h = function()
    if c then
        debug.sethook(nil, "r")
    end
    c = not c
    error()
  end
  debug.sethook(h, "r")
end

print(pcall(pcall, f))
print(pcall(pcall, (function() end)()))
