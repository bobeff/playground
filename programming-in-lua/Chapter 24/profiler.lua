--[[
    Implement some of the suggested improvments for the basic profiler in
Section 24.3
]]

local counters = {}
local names = {}

local function hook()
    local f = debug.getinfo(2, "f").func
    local count = counters[f]
    if count == nil then -- first time 'f' is called ?
        counters[f] = 1
        names[f] = debug.getinfo(2, "Sn")
    else -- only increment the counter
        counters[f] = count + 1
    end
end

local function getname(func)
    local n = names[func]
    if n.what == "C" then
        return n.name
    end
    local lc = string.format("[%s]:%d", n.short_src, n.linedefined)
    if n.what ~= "main" and n.namewhat ~= "" then
        return string.format("%s (%s)", lc, n.name)
    else
        return lc
    end
end

if not arg[1] then
    io.write(string.format("Usage: %s <lua_source_file_name>\n", arg[0]))
    os.exit(0)
end

local f, err = loadfile(arg[1])
if not f then
    print(err)
    os.exit(0)
end

debug.sethook(hook, "c")
f()
debug.sethook()

local funcsArray = {}
for f, _ in pairs(counters) do
    funcsArray[#funcsArray + 1] = f
end

table.sort(funcsArray, function(f1, f2)
    return counters[f1] > counters[f2] end)

for index, func in ipairs(funcsArray) do
    print(index, getname(func), counters[func])
end
